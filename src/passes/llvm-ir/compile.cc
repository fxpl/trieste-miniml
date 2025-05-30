#include "../../miniml-lang.hh"
#include "../internal.hh"
#include "../llvm_utils.hh"
#include "../utils.hh"
#include "trieste/token.h"

namespace miniml {

  using namespace trieste;

  struct CompileContext {
    ~CompileContext() {}
  };

  /**
   * @brief
   *
   * It works similar to denotational semantics where each miniML
   * expression is mapped to its meaning in LLVM IR.
   *
   * [[_]]t : miniML -> LLVM IR
   *
   * [[e1 + e2]]t0 =
   *  [[e1]]t1 : [[e2]]t2 =
   *    add i32 [[e1]]t1, [[e2]]t2
   *
   */
  PassDef compile() {
    auto context = std::make_shared<CompileContext>();

    return {
      "compile",
      LLVMIRCompilation::wf,
      (dir::topdown),
      {
        /**
         * Add compile node and assign tmp/register.
         */
        In(Top) * Start * T(IRProgram)[Program] * End >> [](Match& _) -> Node {
          Node result = Ident ^ _(Program)->fresh();

          return (Compile << result << _(Program));
        },

        /**
         * Compile program and add compile nodes to its children.
         */
        T(Compile) << T(Ident)[Ident] * T(IRProgram)[Program] >>
          [](Match& _) -> Node {
          Node prog = _(Program);

          for (size_t i = 0; i < prog->size(); i++) {
            Node topexpr = prog->at(i);
            if (topexpr == TopExpr || topexpr == Expr) {
              Node ident = Ident ^ prog->fresh();
              prog->replace_at(i, Compile << ident << topexpr);
            } else {
              prog->replace_at(i, Compile << topexpr);
            }
          }

          auto topExpressions = *_(Program);
          prog->erase(prog->begin(), prog->end());

          return Seq << _(Ident)->clone() << (prog << topExpressions);
        },

        /**
         * Compile TopExpression.
         */
        T(Compile) << T(Ident)[Result] * (T(TopExpr) << T(Expr, Let)[Expr]) >>
          [](Match& _) -> Node { return (Compile << _(Result) << _(Expr)); },

        /**
         * Let
         */
        T(Compile) << T(Ident)[Result] * T(Let)[Let] >>
          [context](Match& _) -> Node {
          // TODO: Need to deal with ForAllTy. Until I figure out how, just
          // pretend it cannot be nested and ignore it.
          Node let = _(Let);
          Node ident = let / Ident;
          Node expr = let / Expr;
          Node type = get_type(get_type(let));

          Node llvmType = getLLVMType(type);
          if (llvmType == nullptr) {
            return err(_(Type), "let type not supported");
          }

          return Seq << (Instr << (MemoryOp << (Alloca << ident << llvmType)))
                     << (Compile << _(Result) << expr)
                     << (Instr
                         << (MemoryOp
                             << (Store << _(Result)->clone()
                                       << ident->clone())));
        },

        /**
         * Boolean
         */
        T(Compile) << T(Ident)[Result] *
              (T(Expr) << (T(Type) << T(TBool)[Type]) *
                 T(True, False)[IRValue]) >>
          [](Match& _) -> Node {
          Node value = NULL;
          if (_(IRValue) == True) {
            value = IRValue ^ "1";
          } else if (_(IRValue) == False) {
            value = IRValue ^ "0";
          }
          assert(value);

          Node llvmType = getLLVMType(_(Type));
          assert(llvmType);

          return Action << (CreateConst << _(Result) << llvmType << value);
        },

        /**
         * Integer
         */
        T(Compile) << T(Ident)[Result] *
              (T(Expr) << (T(Type) << T(TInt)[Type]) * T(Int)[Int]) >>
          [](Match& _) -> Node {
          Node value = IRValue ^ node_val(_(Int));

          Node llvmType = getLLVMType(_(Type));
          assert(llvmType);

          return Action << (CreateConst << _(Result) << llvmType << value);
        },

        /**
         * Identifier
         */
        T(Compile)
            << (T(Ident)[Result] *
                (T(Expr) << (T(Type)[Type] * T(Ident)[Ident]))) >>
          [context](Match& _) -> Node {
          auto llvmType = getLLVMType(_(Type) / Type);
          assert(llvmType);

          return Instr
            << (ConversionOp << (BitCast << _(Result) << _(Ident) << llvmType));
        },

        /**
         * Global
         */
        T(Compile)
            << (T(Ident)[Result] *
                (T(Expr) << (T(Type)[Type] * T(Global)[Global]))) >>
          [context](Match& _) -> Node {
          auto llvmType = getLLVMType(_(Type) / Type);
          assert(llvmType);

          return Instr
            << (MemoryOp
                << (Load << _(Result) << llvmType
                         << (Ident ^ node_val(_(Global)))));
        },

        /**
         * Binary operation.
         */
        T(Compile) << T(Ident)[Result] *
              (T(Expr) << T(Type)[Type] * T(Add, Sub, Mul)[BinaryOp]) >>
          [](Match& _) -> Node {
          Node resultId = _(Result);
          Node binop = _(BinaryOp);
          Node lhs = binop / Lhs;
          Node rhs = binop / Rhs;

          Node op = NULL;
          if (binop == Add) {
            op = Add;
          } else if (binop == Sub) {
            op = Sub;
          } else if (binop == Mul) {
            op = Mul;
          }
          assert(op);

          Node llvmType = getLLVMType(_(Type) / Type);
          assert(llvmType);

          Node lhsId = Ident ^ lhs->fresh();
          Node rhsId = Ident ^ rhs->fresh();

          // Instruction
          return Seq << (Compile << lhsId->clone() << lhs)
                     << (Compile << rhsId->clone() << rhs)
                     << (Instr
                         << (BinaryOp
                             << (op << resultId << llvmType << lhsId
                                    << rhsId)));
        },

        /**
         * Comparison
         */
        T(Compile) << T(Ident)[Result] *
              (T(Expr) << T(Type) * T(Equals, LT)[Op]) >>
          [](Match& _) -> Node {
          Node comparison = _(Op);
          Node lhs = comparison / Lhs;
          Node rhs = comparison / Rhs;

          if (get_type(lhs)->type() != get_type(rhs)->type()) {
            return err(comparison, "comparison operands have different types");
          }

          Node type = get_type(lhs);
          Node llvmType = getLLVMType(type);
          if (llvmType == nullptr) {
            return err(lhs / Type, "comparison type not supported");
          }

          Node op = NULL;
          if (comparison == Equals) {
            op = EQ;
          } else if (comparison == LT) {
            op = ULT;
          } else {
            return err(comparison, "comparison operator not supported");
          }

          Node lhsIdent = Ident ^ lhs->fresh();
          Node rhsIdent = Ident ^ rhs->fresh();

          return Seq << (Compile << lhsIdent->clone() << lhs)
                     << (Compile << rhsIdent->clone() << rhs)
                     << (Instr
                         << (MiscOp
                             << (Icmp << _(Result) << op << llvmType << lhsIdent
                                      << rhsIdent)));
        },

        /**
         * If-then-else
         */
        T(Compile) << T(Ident)[Ident] *
              (T(Expr)[Expr] << T(Type) *
                 (T(If) << T(Expr)[Cond] * T(Expr)[True] * T(Expr)[False])) >>
          [](Match& _) -> Node {
          Node type = get_type(_(Expr));
          Node llvmType = getLLVMType(type);
          if (type == nullptr) {
            return err(_(Type), "if-then-else type not supported");
          }
          assert(llvmType);

          Node condId = Ident ^ _(Cond)->fresh();
          Node ifTrueId = Ident ^ _(True)->fresh();
          Node ifFalseId = Ident ^ _(False)->fresh();

          std::string ifId = node_val(condId);
          Node thenLabel = Label ^ ("then" + ifId);
          Node thenEndLabel = Label ^ ("thenEnd" + ifId);
          Node elseLabel = Label ^ ("else" + ifId);
          Node elseEndLabel = Label ^ ("elseEnd" + ifId);
          Node ifEndLabel = Label ^ ("ifEnd" + ifId);

          // clang-format off
          return Seq
            << (Compile << condId << _(Cond))
            // If
            << (Instr
                << (TerminatorOp
                    << (Branch << condId->clone() 
                               << thenLabel->clone()
                               << elseLabel->clone())))
            // Then
            << (thenLabel) 
            << (Compile << ifTrueId->clone() << _(True))
            << (Instr 
                << (TerminatorOp 
                    << (Jump << thenEndLabel->clone())))
            // "Landing" block so Phi unaffected by branching in Then/Else expr.
            << (thenEndLabel)
            << (Instr 
                << (TerminatorOp 
                    << (Jump << ifEndLabel->clone())))
            // Else
            << (elseLabel) 
            << (Compile << ifFalseId->clone() << _(False))
            << (Instr 
                << (TerminatorOp 
                    << (Jump << elseEndLabel->clone())))
            // "Landing" block so Phi unaffected by branching in Then/Else expr.
            << (elseEndLabel)
            << (Instr 
                << (TerminatorOp 
                    << (Jump << ifEndLabel->clone())))
            // ifEnd
            << (ifEndLabel)
            << (Instr
                << (MiscOp
                    << (Phi
                        << _(Ident) << llvmType
                        << (PredecessorList
                            << (Predecessor << ifTrueId << thenEndLabel->clone())
                            << (Predecessor << ifFalseId << elseEndLabel->clone())))));
          // clang-format on
        },

        /**
         * Function Call
         */
        T(Compile) << T(Ident)[Result] *
              (T(Expr) << T(Type) * T(FunCall)[FunCall]) >>
          [](Match& _) -> Node {
          Node funcall = _(FunCall);
          Node fun = funcall / Fun;
          Node param = funcall / Param;

          Node argIdent = Ident ^ param->fresh();
          Node funIdent = Ident ^ fun->fresh();

          return Seq << (Compile << argIdent->clone() << param)
                     << (Compile << funIdent->clone() << fun)
                     << (Instr
                         << (MiscOp
                             << (Call << _(Result) << funIdent
                                      << (ArgList << argIdent))));
        },

        /**
         * Closure Call
         */
        T(Compile) << T(Ident)[Result] *
              (T(Expr) << (T(Type)) * T(ClosureCall)[ClosureCall]) >>
          [](Match& _) -> Node {
          Node closCall = _(ClosureCall);
          Node fun = closCall / Fun;
          Node param = closCall / Param;
          Node typeArrow = get_type(fun);
          Node funArgType = typeArrow / Ty1;
          Node funRetType = typeArrow / Ty2;

          Node argLLVMType = getLLVMType(funArgType);
          assert(argLLVMType);
          Node retLLVMType = getLLVMType(funRetType);
          assert(retLLVMType);

          std::string uniqueId = std::string(fun->fresh().view());
          Node closurePtr = Ident ^ "closPtr_" + uniqueId;
          Node closureTy = Ident ^ "ClosureTy";
          Node funSlotPtr = Ident ^ "funSlot_" + uniqueId;
          Node funPtr = Ident ^ "funPtr_" + uniqueId;
          Node funTy = Ident ^ "funType_" + uniqueId;
          Node envSlotPtr = Ident ^ "envSlot_" + uniqueId;
          Node envPtr = Ident ^ "envPtr_" + uniqueId;
          Node argId = Ident ^ param->fresh();
          Node funId = Ident ^ param->fresh();

          // clang-format off
          Node GEPEnvSlotInClosure = (Instr
              << (MemoryOp
                  << (GetElementPtr
                      << envSlotPtr 
                      << closureTy 
                      << closurePtr->clone()
                      << (OffsetList
                          << (Offset << Ti32 << (IRValue ^ "0"))
                          << (Offset << Ti32 << (IRValue ^ "0"))))));
          Node loadEnvPtr = (Instr
              << (MemoryOp
                  << (Load << envPtr << TPtr << envSlotPtr->clone())));
          Node GEPFunSlotInClosure = (Instr
              << (MemoryOp
                  << (GetElementPtr
                      << funSlotPtr 
                      << closureTy->clone()
                      << closurePtr->clone()
                      << (OffsetList
                          << (Offset << Ti32 << (IRValue ^ "0"))
                          << (Offset << Ti32 << (IRValue ^ "1"))))));
          Node loadFunPtr = (Instr
              << (MemoryOp
                  << (Load << funPtr << TPtr << funSlotPtr->clone())));
          Node createFunType = (Action
              << (CreateFunType
                  << funTy << retLLVMType
                  << (IRTypeList << TPtr << TPtr << argLLVMType)));
          Node callFun = (Instr
              << (MiscOp
                  << (CallOpaque
                      << _(Result) << funTy->clone() << funPtr->clone()
                      << (ArgList << closurePtr->clone() << envPtr->clone()
                                  << argId->clone()))));

          return Seq
            << (Compile << closurePtr << fun)
            << GEPEnvSlotInClosure
            << loadEnvPtr
            << GEPFunSlotInClosure
            << loadFunPtr
            << (Compile << argId << param)
            << createFunType
            << callFun;
          // clang-format on
        },

        /**
         * Print
         */
        T(Compile) << T(Ident)[Ident] *
              (T(Expr) << (T(Type) << T(TypeArrow)[TypeArrow]) * T(Print)) >>
          [](Match& _) -> Node {
          Node funcName = NULL;
          if ((_(TypeArrow) / Ty1)->type() != (_(TypeArrow) / Ty2)->type()) {
            return err(_(TypeArrow), "print is an identity function");
          }

          if ((_(TypeArrow) / Ty1 == TInt) && (_(TypeArrow) / Ty2 == TInt)) {
            funcName = Ident ^ "native$printInt";
          } else if (
            (_(TypeArrow) / Ty1 == TBool) && (_(TypeArrow) / Ty2 == TBool)) {
            funcName = Ident ^ "native$printBool";
          }

          if (!funcName) {
            return err(_(TypeArrow), "print function with this type not found");
          }

          return (Action << (GetFunction << _(Ident) << funcName));
        },

        /**
         * Function Declaration
         */
        T(Compile) << T(IRFun)[IRFun] >> [](Match& _) -> Node {
          Node fun = _(IRFun);
          Node paramList = fun / ParamList;
          Node freeVarList = fun / FreeVarList;
          Node body = fun / Body;

          // TODO: Support polymorphism (Tvar)
          Node type = get_type(fun);
          Node paramType = getLLVMType(type / Ty1);
          assert(paramType);
          Node returnType = getLLVMType(type / Ty2);
          assert(returnType);

          Node funType = TypeArrow << paramType << returnType;

          std::string uniqueId = std::string(fun->fresh().view());
          Node entryPoint = Label ^ ("entry_" + uniqueId);
          Node returnId = Ident ^ ("ret_" + uniqueId);

          Node irfun = IRFun ^ node_val(fun);
          Node returnInstr =
            (Instr << (TerminatorOp << (Ret << returnId->clone())));

          if ("main" == node_val(fun)) {
            // clang-format off
            return Seq
              << (irfun
                << funType
                << (Compile << paramList)
                << (Body
                    << entryPoint
                    // main() may contain multiple expressions.
                    << (Compile << (PropagateCompile << *body))
                    // main() expected to return 0 for program success.
                    << (Action
                        << (CreateConst << returnId->clone() << Ti32
                                        << (IRValue ^ "0")))
                    << returnInstr));
            // clang-format on
          } else {
            Node env = fun / Env;

            Node envType = Ident ^ node_val(env);
            Node envPtr = (paramList->at(1) / Ident)->clone();
            Node loadFreeVarsFromEnv =
              (Compile << envType->clone() << envPtr->clone() << freeVarList
                       << Load);

            // clang-format off
            return Seq
              << (irfun
                  << funType
                  << (Compile << paramList)
                  << (Body
                      << entryPoint
                      << loadFreeVarsFromEnv
                      << (Compile << returnId << *body)
                      // MiniML has implicit return so must insert it here.
                      << returnInstr));
            // clang-format on
          }
        },

        /**
         * Environment
         */
        T(Compile) << T(Env)[Env] >> [](Match& _) -> Node {
          Node env = _(Env);
          if (env->size() == 0) {
            return {};
          }

          for (size_t i = 0; i < env->size(); i++) {
            Node type = env->at(i);
            env->replace_at(i, (Compile << type));
          }

          Node name = Ident ^ node_val(env);
          auto typesToCompile = *env;

          return Action
            << (CreateStructType << name << (IRTypeList << typesToCompile));
        },

        /**
         * Create Closure
         */
        T(Compile)
            << (T(Ident)[Result] *
                (T(Expr) << (T(Type) * (T(CreateClosure)[CreateClosure])))) >>
          [](Match& _) -> Node {
          Node createClosure = _(CreateClosure);
          Node fun = createClosure / Fun;
          Node freeVarList = createClosure / FreeVarList;

          Node allocatorFun = Ident ^ "malloc";

          // FIXME: Assumes a ptr is i64 = 8 Bytes.
          size_t closureByteCount = 20;
          Node closureBytesValue = IRValue ^ std::to_string(closureByteCount);
          Node closureBytes = Ident ^ createClosure->fresh();
          Node closurePtr = _(Result);
          Node closureTy = Ident ^ "ClosureTy";

          std::string uniqueId = std::string(createClosure->fresh().view());

          Node envSlotPtr = Ident ^ "envSlot_" + uniqueId;
          Node envPtr = Ident ^ "envPtr_" + uniqueId;

          Node funSlotPtr = Ident ^ "funSlot_" + uniqueId;
          Node funPtr = Ident ^ "funPtr_" + uniqueId;

          // clang-format off
          Node createClosByteSize =
            (Action
             << (CreateConst << closureBytes << Ti64 << closureBytesValue));
          Node allocateClosure =
            (Instr
             << (MiscOp
                 << (Call << closurePtr
                          << allocatorFun->clone()
                          << (ArgList << closureBytes->clone()))));
          Node GEPFunSlotInClosure =
            (Instr
             << (MemoryOp
                 << (GetElementPtr
                     << funSlotPtr << closureTy->clone() << closurePtr->clone()
                     << (OffsetList << (Offset << Ti32 << (IRValue ^ "0"))
                                    << (Offset << Ti32 << (IRValue ^ "1"))))));
          Node getFunPtr = (Action << (GetFunction << funPtr << fun));
          Node storeFunPtrInClosure =
            (Instr
             << (MemoryOp
                 << (Store << funPtr->clone() << funSlotPtr->clone())));
          // clang-format on

          if (freeVarList->size() > 0) {
            Node env = createClosure / Env;

            // FIXME: Calculate env size based on FreeVarList.
            size_t envByteCount = 10;

            Node envBytesValue = IRValue ^ std::to_string(envByteCount);
            Node envBytes = Ident ^ createClosure->fresh();
            Node envTy = Ident ^ node_val(env);

            // clang-format off
            Node createEnvByteSize =
              (Action << (CreateConst << envBytes << Ti64 << envBytesValue));
            Node allocateEnv =
              (Instr
               << (MiscOp
                   << (Call << envPtr
                            << allocatorFun
                            << (ArgList << envBytes->clone()))));
            Node storeFreeVarsInEnv =
              (Compile
                << envTy->clone() << envPtr->clone() << freeVarList << Store);
            Node GEPEnvSlotInClosure =
              (Instr
                << (MemoryOp
                    << (GetElementPtr
                        << envSlotPtr
                        << closureTy
                        << closurePtr->clone()
                        << (OffsetList << (Offset << Ti32 << (IRValue ^ "0"))
                                       << (Offset << Ti32 << (IRValue ^ "0"))))));
            Node storeEnvPtrInClosure =
              (Instr
              << (MemoryOp
                  << (Store << envPtr->clone() << envSlotPtr->clone())));

            return Seq << createClosByteSize
                       << allocateClosure
                       << createEnvByteSize
                       << allocateEnv
                       << storeFreeVarsInEnv
                       << GEPEnvSlotInClosure
                       << storeEnvPtrInClosure
                       << GEPFunSlotInClosure
                       << getFunPtr
                       << storeFunPtrInClosure;
          } else {
            return Seq << createClosByteSize
                       << allocateClosure
                       << GEPFunSlotInClosure
                       << getFunPtr
                       << storeFunPtrInClosure;
          }
          // clang-format on
        },

        /**
         * Type
         */
        T(Compile) << T(Type)[Type] >> [](Match& _) -> Node {
          Node type = _(Type) / Type;
          Node llvmType = getLLVMType(type);
          if (llvmType == nullptr) {
            return err(type, "Cannot convert to equivalent LLVM IR type");
          }

          return llvmType;
        },

        /**
         * ParamList
         */
        T(Compile) << T(ParamList)[ParamList] >> [](Match& _) -> Node {
          Node paramList = _(ParamList);

          for (size_t i = 0; i < paramList->size(); i++) {
            Node param = paramList->at(i);
            paramList->replace_at(i, (Compile << param));
          }

          return paramList;
        },

        /**
         * Parameter
         */
        T(Compile) << T(Param)[Param] >> [](Match& _) -> Node {
          Node param = _(Param);
          Node ident = param / Ident;
          Node type = get_type(param);

          Node llvmType = getLLVMType(type);
          assert(llvmType);

          return Param << ident << llvmType;
        },

        /**
         * FreeVarList (Populate environment)
         */
        T(Compile) << T(Ident)[Env] * T(Ident)[TPtr] *
              T(FreeVarList)[FreeVarList] * T(Store) >>
          [](Match& _) -> Node {
          Node envPtr = _(TPtr);
          Node envTy = _(Env);
          Node freeVarList = _(FreeVarList);

          Node seq = Seq;
          for (size_t i = 0; i < freeVarList->size(); i++) {
            Node freeVar = freeVarList->at(i);
            Node ident = freeVar / Ident;
            Node type = getLLVMType(get_type(freeVar));

            Node tmp = Ident ^ freeVarList->fresh();
            Node freeVarSlot = Ident ^ freeVarList->fresh();

            // clang-format off
            Node getFreeVar = nullptr;
            if (ident == Global) {
              Node loadInstr =
                (Instr
                 << (MemoryOp
                     << (Load << tmp
                              << type->clone()
                              << (Ident ^ node_val(ident)))));

              getFreeVar = loadInstr;
            } else {
              Node bitcastInstr =
                (Instr
                 << (ConversionOp
                     << (BitCast << tmp
                                 << ident->clone()
                                 << type->clone())));

              getFreeVar = bitcastInstr;
            }

            Node GEPFreeVarSlotInEnv =
              (Instr
               << (MemoryOp
                   << (GetElementPtr
                       << freeVarSlot << envTy->clone() << envPtr->clone()
                       << (OffsetList
                           << (Offset << Ti32 << (IRValue ^ "0"))
                           << (Offset << Ti32 << (IRValue ^ std::to_string(i)))))));
            Node storeFreeVarInEnv =
              (Instr
               << (MemoryOp
                   << (Store << tmp->clone() << freeVarSlot->clone())));

            seq << getFreeVar
                << GEPFreeVarSlotInEnv
                << storeFreeVarInEnv;
            // clang-format on
          }

          return seq;
        },

        /**
         * FreeVarList (Environment -> Registers)
         */
        T(Compile) << T(Ident)[Env] * T(Ident)[TPtr] *
              T(FreeVarList)[FreeVarList] * T(Load) >>
          [](Match& _) -> Node {
          Node envPtr = _(TPtr);
          Node envTy = _(Env);
          Node freeVarList = _(FreeVarList);

          Node seq = Seq;
          for (size_t i = 0; i < freeVarList->size(); i++) {
            Node freeVar = freeVarList->at(i);
            Node ident = freeVar / Ident;
            Node type = getLLVMType(get_type(freeVar));

            Node tmp = Ident ^ freeVarList->fresh();
            Node freeVarSlot = Ident ^ freeVarList->fresh();

            // clang-format off
            Node GEPFreeVarSlotInEnv =
              (Instr
               << (MemoryOp
                   << (GetElementPtr
                       << freeVarSlot->clone()
                       << envTy->clone()
                       << envPtr->clone()
                       << (OffsetList
                           << (Offset << Ti32 << (IRValue ^ "0"))
                           << (Offset << Ti32 << (IRValue ^ std::to_string(i)))))));
            Node loadFreeVarFromEnv =
              (Instr
               << (MemoryOp
                   << (Load << tmp->clone() << type->clone() << freeVarSlot->clone())));
            Node allocaFreeVar =
              (Instr
               << (MemoryOp
                   << (Alloca << ident->clone() << type->clone())));
            Node storeFreeVarOnStack =
              (Instr
               << (MemoryOp
                   << (Store << tmp->clone() << ident->clone())));

            seq << GEPFreeVarSlotInEnv
                << loadFreeVarFromEnv
                << allocaFreeVar
                << storeFreeVarOnStack;
            // clang-format on
          }

          return seq;
        },

        // Propagate Compile onto several nodes.
        T(Compile) << T(PropagateCompile)[PropagateCompile] >>
          [](Match& _) -> Node {
          for (size_t i = 0; i < _(PropagateCompile)->size(); i++) {
            Node child = _(PropagateCompile)->at(i);
            Node ident = Ident ^ _(PropagateCompile)->fresh();
            _(PropagateCompile)->replace_at(i, Compile << ident << child);
          }

          auto children = *_(PropagateCompile);

          return Seq << children;
        },
      }};
  }
}
