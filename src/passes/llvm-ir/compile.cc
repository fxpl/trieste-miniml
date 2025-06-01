#include "../../llvm-lang.hh"
#include "../../miniml-lang.hh"
#include "../internal.hh"
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
          return (Compile << (llvmir::Program << *_(Program)));
        },

        /**
         * Compile program and add compile nodes to its children.
         */
        T(Compile) << T(llvmir::Program)[Program] >> [](Match& _) -> Node {
          Node prog = _(Program);

          for (size_t i = 0; i < prog->size(); i++) {
            Node topexpr = prog->at(i);
            if (topexpr == TopExpr || topexpr == Expr) {
              Node ident = llvmir::Ident ^ prog->fresh();
              prog->replace_at(i, Compile << ident << topexpr);
            } else {
              prog->replace_at(i, Compile << topexpr);
            }
          }

          auto topExpressions = *_(Program);
          prog->erase(prog->begin(), prog->end());

          return Seq << (prog << topExpressions);
        },

        /**
         * Compile TopExpression.
         */
        T(Compile) << T(llvmir::Ident)[Result] *
              (T(TopExpr) << T(Expr, Let)[Expr]) >>
          [](Match& _) -> Node { return (Compile << _(Result) << _(Expr)); },

        /**
         * Let
         */
        T(Compile) << T(llvmir::Ident)[Result] * T(Let)[Let] >>
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

          Node irIdent = llvmir::Ident ^ node_val(ident);

          return Seq << (llvmir::Instr
                         << (llvmir::MemoryOp
                             << (llvmir::Alloca << irIdent << llvmType)))
                     << (Compile << _(Result) << expr)
                     << (llvmir::Instr
                         << (llvmir::MemoryOp
                             << (llvmir::Store << _(Result)->clone()
                                               << irIdent->clone())));
        },

        /**
         * Boolean
         */
        T(Compile) << T(llvmir::Ident)[Result] *
              (T(Expr) << (T(Type) << T(TBool)[Type]) *
                 T(True, False)[TBool]) >>
          [](Match& _) -> Node {
          Node irValue = _(TBool);
          Node value = NULL;
          if (irValue == True) {
            value = llvmir::IRValue ^ "1";
          } else if (irValue == False) {
            value = llvmir::IRValue ^ "0";
          }
          assert(value);

          Node llvmType = getLLVMType(_(Type));
          assert(llvmType);

          return llvmir::Action
            << (llvmir::CreateConst << _(Result) << llvmType << value);
        },

        /**
         * Integer
         */
        T(Compile) << T(llvmir::Ident)[Result] *
              (T(Expr) << (T(Type) << T(TInt)[Type]) * T(Int)[Int]) >>
          [](Match& _) -> Node {
          Node value = llvmir::IRValue ^ node_val(_(Int));

          Node llvmType = getLLVMType(_(Type));
          assert(llvmType);

          return llvmir::Action
            << (llvmir::CreateConst << _(Result) << llvmType << value);
        },

        /**
         * Identifier
         */
        T(Compile)
            << (T(llvmir::Ident)[Result] *
                (T(Expr) << (T(Type)[Type] * T(Ident)[Ident]))) >>
          [context](Match& _) -> Node {
          auto llvmType = getLLVMType(_(Type) / Type);
          assert(llvmType);

          Node irIdent = llvmir::Ident ^ node_val(_(Ident));

          return llvmir::Instr
            << (llvmir::ConversionOp
                << (llvmir::BitCast << _(Result) << irIdent << llvmType));
        },

        /**
         * Global
         */
        T(Compile)
            << (T(llvmir::Ident)[Result] *
                (T(Expr) << (T(Type)[Type] * T(Global)[Global]))) >>
          [context](Match& _) -> Node {
          auto llvmType = getLLVMType(_(Type) / Type);
          assert(llvmType);

          return llvmir::Instr
            << (llvmir::MemoryOp
                << (llvmir::Load << _(Result) << llvmType
                                 << (llvmir::Ident ^ node_val(_(Global)))));
        },

        /**
         * Binary operation.
         */
        T(Compile) << T(llvmir::Ident)[Result] *
              (T(Expr) << T(Type)[Type] * T(Add, Sub, Mul)[Op]) >>
          [](Match& _) -> Node {
          Node resultId = _(Result);
          Node binop = _(Op);
          Node lhs = binop / Lhs;
          Node rhs = binop / Rhs;

          Node op = NULL;
          if (binop == Add) {
            op = llvmir::Add;
          } else if (binop == Sub) {
            op = llvmir::Sub;
          } else if (binop == Mul) {
            op = llvmir::Mul;
          }
          assert(op);

          Node llvmType = getLLVMType(_(Type) / Type);
          assert(llvmType);

          Node lhsId = llvmir::Ident ^ lhs->fresh();
          Node rhsId = llvmir::Ident ^ rhs->fresh();

          // Instruction
          return Seq << (Compile << lhsId->clone() << lhs)
                     << (Compile << rhsId->clone() << rhs)
                     << (llvmir::Instr
                         << (llvmir::BinaryOp
                             << (op << resultId << llvmType << lhsId
                                    << rhsId)));
        },

        /**
         * Comparison
         */
        T(Compile) << T(llvmir::Ident)[Result] *
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
            op = llvmir::EQ;
          } else if (comparison == LT) {
            op = llvmir::SLT;
          } else {
            return err(comparison, "comparison operator not supported");
          }

          Node lhsIdent = llvmir::Ident ^ lhs->fresh();
          Node rhsIdent = llvmir::Ident ^ rhs->fresh();

          return Seq << (Compile << lhsIdent->clone() << lhs)
                     << (Compile << rhsIdent->clone() << rhs)
                     << (llvmir::Instr
                         << (llvmir::MiscOp
                             << (llvmir::Icmp << _(Result) << op << llvmType
                                              << lhsIdent << rhsIdent)));
        },

        /**
         * If-then-else
         */
        T(Compile) << T(llvmir::Ident)[Result] *
              (T(Expr)[Expr] << T(Type) *
                 (T(If) << T(Expr)[Cond] * T(Expr)[True] * T(Expr)[False])) >>
          [](Match& _) -> Node {
          Node type = get_type(_(Expr));
          Node llvmType = getLLVMType(type);
          if (type == nullptr) {
            return err(_(Type), "if-then-else type not supported");
          }
          assert(llvmType);

          Node condId = llvmir::Ident ^ _(Cond)->fresh();
          Node ifTrueId = llvmir::Ident ^ _(True)->fresh();
          Node ifFalseId = llvmir::Ident ^ _(False)->fresh();

          std::string ifId = node_val(condId);
          Node thenLabel = llvmir::Label ^ ("then" + ifId);
          Node thenEndLabel = llvmir::Label ^ ("thenEnd" + ifId);
          Node elseLabel = llvmir::Label ^ ("else" + ifId);
          Node elseEndLabel = llvmir::Label ^ ("elseEnd" + ifId);
          Node ifEndLabel = llvmir::Label ^ ("ifEnd" + ifId);

          // clang-format off
          return Seq
            << (Compile << condId << _(Cond))
            << (llvmir::Instr
                << (llvmir::TerminatorOp
                    << (llvmir::Branch
                        << condId->clone()
                        << thenLabel->clone()
                        << elseLabel->clone())))
            << (thenLabel)
            << (Compile << ifTrueId->clone() << _(True))
            << (llvmir::Instr
                << (llvmir::TerminatorOp
                    << (llvmir::Jump << thenEndLabel->clone())))
            // "Landing" block so Phi unaffected by branching in Then/Else expr.
            << (thenEndLabel)
            << (llvmir::Instr
                << (llvmir::TerminatorOp
                    << (llvmir::Jump << ifEndLabel->clone())))
            << (elseLabel)
            << (Compile << ifFalseId->clone() << _(False))
            << (llvmir::Instr
                << (llvmir::TerminatorOp
                    << (llvmir::Jump << elseEndLabel->clone())))
            // "Landing" block so Phi unaffected by branching in Then/Else expr.
            << (elseEndLabel)
            << (llvmir::Instr
                << (llvmir::TerminatorOp
                    << (llvmir::Jump << ifEndLabel->clone())))
            << (ifEndLabel)
            << (llvmir::Instr
                << (llvmir::MiscOp
                    << (llvmir::Phi
                        << _(Result)
                        << llvmType
                        << (llvmir::PredecessorList
                            << (llvmir::Predecessor << ifTrueId << thenEndLabel->clone())
                            << (llvmir::Predecessor << ifFalseId << elseEndLabel->clone())))));
          // clang-format on
        },

        /**
         * Function Call
         */
        T(Compile) << T(llvmir::Ident)[Result] *
              (T(Expr) << T(Type) * T(FunCall)[FunCall]) >>
          [](Match& _) -> Node {
          Node funcall = _(FunCall);
          Node fun = funcall / Fun;
          Node param = funcall / Param;

          Node argIdent = llvmir::Ident ^ param->fresh();
          Node funIdent = llvmir::Ident ^ fun->fresh();

          return Seq << (Compile << argIdent->clone() << param)
                     << (Compile << funIdent->clone() << fun)
                     << (llvmir::Instr
                         << (llvmir::MiscOp
                             << (llvmir::Call
                                 << _(Result) << funIdent
                                 << (llvmir::ArgList << argIdent))));
        },

        /**
         * Closure Call
         */
        T(Compile) << T(llvmir::Ident)[Result] *
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
          Node closurePtr = llvmir::Ident ^ "closPtr_" + uniqueId;
          Node closureTy = llvmir::Ident ^ "ClosureTy";
          Node funSlotPtr = llvmir::Ident ^ "funSlot_" + uniqueId;
          Node funPtr = llvmir::Ident ^ "funPtr_" + uniqueId;
          Node funTy = llvmir::Ident ^ "funType_" + uniqueId;
          Node envSlotPtr = llvmir::Ident ^ "envSlot_" + uniqueId;
          Node envPtr = llvmir::Ident ^ "envPtr_" + uniqueId;
          Node argId = llvmir::Ident ^ param->fresh();
          Node funId = llvmir::Ident ^ param->fresh();

          // clang-format off
          Node GEPEnvSlotInClosure =
            (llvmir::Instr
             << (llvmir::MemoryOp
                 << (llvmir::GetElementPtr
                     << envSlotPtr
                     << closureTy
                     << closurePtr->clone()
                     << (llvmir::OffsetList
                         << (llvmir::Offset << llvmir::Ti32 << (llvmir::IRValue ^ "0"))
                         << (llvmir::Offset << llvmir::Ti32 << (llvmir::IRValue ^ "0"))))));
          Node loadEnvPtr =
            (llvmir::Instr
             << (llvmir::MemoryOp
                 << (llvmir::Load << envPtr << llvmir::TPtr << envSlotPtr->clone())));
          Node GEPFunSlotInClosure =
            (llvmir::Instr
             << (llvmir::MemoryOp
                 << (llvmir::GetElementPtr
                     << funSlotPtr
                     << closureTy->clone()
                     << closurePtr->clone()
                     << (llvmir::OffsetList
                         << (llvmir::Offset << llvmir::Ti32 << (llvmir::IRValue ^ "0"))
                         << (llvmir::Offset << llvmir::Ti32 << (llvmir::IRValue ^ "1"))))));
          Node loadFunPtr =
            (llvmir::Instr
             << (llvmir::MemoryOp
                 << (llvmir::Load << funPtr << llvmir::TPtr << funSlotPtr->clone())));
          Node createFunType =
            (llvmir::Action
             << (llvmir::CreateFunType
                 << funTy
                 << retLLVMType
                 << (llvmir::TypeList << llvmir::TPtr << llvmir::TPtr << argLLVMType)));
          Node callFun =
            (llvmir::Instr
             << (llvmir::MiscOp
                 << (llvmir::CallOpaque
                     << _(Result) << funTy->clone() << funPtr->clone()
                     << (llvmir::ArgList
                         << closurePtr->clone()
                         << envPtr->clone()
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
        T(Compile) << T(llvmir::Ident)[Result] *
              (T(Expr) << (T(Type) << T(TypeArrow)[TypeArrow]) * T(Print)) >>
          [](Match& _) -> Node {
          Node funcName = NULL;
          if ((_(TypeArrow) / Ty1)->type() != (_(TypeArrow) / Ty2)->type()) {
            return err(_(TypeArrow), "print is an identity function");
          }

          if ((_(TypeArrow) / Ty1 == TInt) && (_(TypeArrow) / Ty2 == TInt)) {
            funcName = llvmir::Ident ^ "native$printInt";
          } else if (
            (_(TypeArrow) / Ty1 == TBool) && (_(TypeArrow) / Ty2 == TBool)) {
            funcName = llvmir::Ident ^ "native$printBool";
          }

          if (!funcName) {
            return err(_(TypeArrow), "print function with this type not found");
          }

          return (
            llvmir::Action << (llvmir::GetFunction << _(Result) << funcName));
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

          Node funType = llvmir::TypeArrow << paramType << returnType;

          std::string uniqueId = std::string(fun->fresh().view());
          Node entryPoint = llvmir::Label ^ ("entry_" + uniqueId);
          Node returnId = llvmir::Ident ^ ("ret_" + uniqueId);

          Node irfun = llvmir::IRFun ^ node_val(fun);
          Node returnInstr =
            (llvmir::Instr
             << (llvmir::TerminatorOp << (llvmir::Ret << returnId->clone())));

          if ("main" == node_val(fun)) {
            // clang-format off
            return Seq
              << (irfun
                  << funType
                  << (Compile << paramList)
                  << (llvmir::Body
                      << entryPoint
                      // main() may contain multiple expressions.
                      << (Compile << (PropagateCompile << *body))
                      // main() expected to return 0 for program success.
                      << (llvmir::Action
                          << (llvmir::CreateConst
                              << returnId->clone()
                              << llvmir::Ti32
                              << (llvmir::IRValue ^ "0")))
                      << returnInstr));
            // clang-format on
          } else {
            Node env = fun / Env;

            Node envType = llvmir::Ident ^ node_val(env);
            Node envPtr = llvmir::Ident ^ node_val((paramList->at(1) / Ident));
            Node loadFreeVarsFromEnv =
              (Compile << envType->clone() << envPtr->clone() << freeVarList
                       << llvmir::Load);

            // clang-format off
            return Seq
              << (irfun
                  << funType
                  << (Compile << paramList)
                  << (llvmir::Body
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

          Node name = llvmir::Ident ^ node_val(env);
          auto typesToCompile = *env;

          return llvmir::Action
            << (llvmir::CreateStructType
                << name << (llvmir::TypeList << typesToCompile));
        },

        /**
         * Create Closure
         */
        T(Compile)
            << (T(llvmir::Ident)[Result] *
                (T(Expr) << (T(Type) * (T(CreateClosure)[CreateClosure])))) >>
          [](Match& _) -> Node {
          Node createClosure = _(CreateClosure);
          Node fun = createClosure / Fun;
          Node irFun = llvmir::Ident ^ node_val(fun);
          Node freeVarList = createClosure / FreeVarList;

          Node allocatorFun = llvmir::Ident ^ "malloc";

          Node closureBytes = llvmir::Ident ^ createClosure->fresh();
          Node closurePtr = _(Result);
          Node closureTy = llvmir::Ident ^ "ClosureTy";

          std::string uniqueId = std::string(createClosure->fresh().view());

          Node envSlotPtr = llvmir::Ident ^ "envSlot_" + uniqueId;
          Node envPtr = llvmir::Ident ^ "envPtr_" + uniqueId;

          Node funSlotPtr = llvmir::Ident ^ "funSlot_" + uniqueId;
          Node funPtr = llvmir::Ident ^ "funPtr_" + uniqueId;

          // clang-format off
          Node getClosureSize =
            (llvmir::Action
             << (llvmir::GetSizeOfType
                 << closureBytes << llvmir::Ti64 << closureTy));
          Node allocateClosure =
            (llvmir::Instr
             << (llvmir::MiscOp
                 << (llvmir::Call
                     << closurePtr
                     << allocatorFun->clone()
                     << (llvmir::ArgList << closureBytes->clone()))));
          Node GEPFunSlotInClosure =
            (llvmir::Instr
             << (llvmir::MemoryOp
                 << (llvmir::GetElementPtr
                     << funSlotPtr
                     << closureTy->clone()
                     << closurePtr->clone()
                     << (llvmir::OffsetList
                         << (llvmir::Offset << llvmir::Ti32 << (llvmir::IRValue ^ "0"))
                         << (llvmir::Offset << llvmir::Ti32 << (llvmir::IRValue ^ "1"))))));
          Node getFunPtr =
            (llvmir::Action
             << (llvmir::GetFunction << funPtr << irFun));
          Node storeFunPtrInClosure =
            (llvmir::Instr
             << (llvmir::MemoryOp
                 << (llvmir::Store << funPtr->clone() << funSlotPtr->clone())));
          // clang-format on

          if (freeVarList->size() > 0) {
            Node env = createClosure / Env;

            Node envBytes = llvmir::Ident ^ createClosure->fresh();
            Node envTy = llvmir::Ident ^ node_val(env);

            // clang-format off
            Node getEnvSize =
              (llvmir::Action
               << (llvmir::GetSizeOfType
                   << envBytes << llvmir::Ti64 << envTy));
            Node allocateEnv =
              (llvmir::Instr
               << (llvmir::MiscOp
                   << (llvmir::Call
                       << envPtr
                       << allocatorFun
                       << (llvmir::ArgList << envBytes->clone()))));
            Node storeFreeVarsInEnv =
              (Compile
               << envTy->clone() << envPtr->clone() << freeVarList << llvmir::Store);
            Node GEPEnvSlotInClosure =
              (llvmir::Instr
               << (llvmir::MemoryOp
                   << (llvmir::GetElementPtr
                       << envSlotPtr
                       << closureTy->clone()
                       << closurePtr->clone()
                       << (llvmir::OffsetList
                           << (llvmir::Offset << llvmir::Ti32 << (llvmir::IRValue ^ "0"))
                           << (llvmir::Offset << llvmir::Ti32 << (llvmir::IRValue ^ "0"))))));
            Node storeEnvPtrInClosure =
              (llvmir::Instr
               << (llvmir::MemoryOp
                   << (llvmir::Store << envPtr->clone() << envSlotPtr->clone())));

            return Seq << getClosureSize
                       << allocateClosure
                       << getEnvSize
                       << allocateEnv
                       << storeFreeVarsInEnv
                       << GEPEnvSlotInClosure
                       << storeEnvPtrInClosure
                       << GEPFunSlotInClosure
                       << getFunPtr
                       << storeFunPtrInClosure;
          } else {
            return Seq << getClosureSize
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

          return (llvmir::ParamList << *paramList);
        },

        /**
         * Parameter
         */
        T(Compile) << T(Param)[Param] >> [](Match& _) -> Node {
          Node param = _(Param);
          Node ident = param / Ident;
          Node irIdent = llvmir::Ident ^ node_val(ident);

          Node type = get_type(param);
          Node llvmType = getLLVMType(type);
          assert(llvmType);

          return llvmir::Param << irIdent << llvmType;
        },

        /**
         * FreeVarList (Populate environment)
         */
        T(Compile) << T(llvmir::Ident)[Env] * T(llvmir::Ident)[TPtr] *
              T(FreeVarList)[FreeVarList] * T(llvmir::Store) >>
          [](Match& _) -> Node {
          Node envPtr = _(TPtr);
          Node envTy = _(Env);
          Node freeVarList = _(FreeVarList);

          Node seq = Seq;
          for (size_t i = 0; i < freeVarList->size(); i++) {
            Node freeVar = freeVarList->at(i);
            Node ident = freeVar / Ident;
            Node irIdent = llvmir::Ident ^ node_val(ident);
            Node type = getLLVMType(get_type(freeVar));

            Node tmp = llvmir::Ident ^ freeVarList->fresh();
            Node freeVarSlot = llvmir::Ident ^ freeVarList->fresh();

            // clang-format off
            Node getFreeVar = nullptr;
            if (ident == Global) {
              Node loadInstr =
                (llvmir::Instr
                 << (llvmir::MemoryOp
                     << (llvmir::Load << tmp << type->clone() << irIdent)));

              getFreeVar = loadInstr;
            } else {
              Node bitcastInstr =
                (llvmir::Instr
                 << (llvmir::ConversionOp
                     << (llvmir::BitCast << tmp << irIdent->clone() << type->clone())));

              getFreeVar = bitcastInstr;
            }

            Node GEPFreeVarSlotInEnv =
              (llvmir::Instr
               << (llvmir::MemoryOp
                   << (llvmir::GetElementPtr
                       << freeVarSlot << envTy->clone() << envPtr->clone()
                       << (llvmir::OffsetList
                           << (llvmir::Offset << llvmir::Ti32
                                              << (llvmir::IRValue ^ "0"))
                           << (llvmir::Offset << llvmir::Ti32
                                              << (llvmir::IRValue ^ std::to_string(i)))))));
            Node storeFreeVarInEnv =
              (llvmir::Instr
               << (llvmir::MemoryOp
                   << (llvmir::Store << tmp->clone() << freeVarSlot->clone())));

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
        T(Compile) << T(llvmir::Ident)[Env] * T(llvmir::Ident)[TPtr] *
              T(FreeVarList)[FreeVarList] * T(llvmir::Load) >>
          [](Match& _) -> Node {
          Node envPtr = _(TPtr);
          Node envTy = _(Env);
          Node freeVarList = _(FreeVarList);

          Node seq = Seq;
          for (size_t i = 0; i < freeVarList->size(); i++) {
            Node freeVar = freeVarList->at(i);
            Node ident = freeVar / Ident;
            Node irIdent = llvmir::Ident ^ node_val(ident);
            Node type = getLLVMType(get_type(freeVar));

            Node tmp = llvmir::Ident ^ freeVarList->fresh();
            Node freeVarSlot = llvmir::Ident ^ freeVarList->fresh();

            // clang-format off
            Node GEPFreeVarSlotInEnv =
              (llvmir::Instr
               << (llvmir::MemoryOp
                   << (llvmir::GetElementPtr
                       << freeVarSlot->clone()
                       << envTy->clone()
                       << envPtr->clone()
                       << (llvmir::OffsetList
                           << (llvmir::Offset << llvmir::Ti32
                                              << (llvmir::IRValue ^ "0"))
                           << (llvmir::Offset << llvmir::Ti32
                                              << (llvmir::IRValue ^ std::to_string(i)))))));
            Node loadFreeVarFromEnv =
              (llvmir::Instr
               << (llvmir::MemoryOp
                   << (llvmir::Load << tmp->clone() << type->clone() << freeVarSlot->clone())));
            Node allocaFreeVar =
              (llvmir::Instr
               << (llvmir::MemoryOp
                   << (llvmir::Alloca << irIdent->clone() << type->clone())));
            Node storeFreeVarOnStack =
              (llvmir::Instr
               << (llvmir::MemoryOp
                   << (llvmir::Store << tmp->clone() << irIdent->clone())));

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
            Node ident = llvmir::Ident ^ _(PropagateCompile)->fresh();
            _(PropagateCompile)->replace_at(i, Compile << ident << child);
          }

          auto children = *_(PropagateCompile);

          return Seq << children;
        },
      }};
  }
}
