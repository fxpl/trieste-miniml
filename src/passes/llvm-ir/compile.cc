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
        In(Top) * Start * T(Program)[Program] * End >> [](Match& _) -> Node {
          Node result = Ident ^ _(Program)->fresh();

          return (Compile << result << _(Program));
        },

        /**
         * Compile program and add compile nodes to its children.
         */
        T(Compile) << T(Ident)[Ident] * T(Program)[Program] >>
          [](Match& _) -> Node {
          // FIXME: Debug print
          std::cout << "Compile" << std::endl;

          Node prog = _(Program);

          // Generate identifiers for each of the program's top expressions.
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

          // TODO: Is this still a good idea?
          //
          //       Top
          //      /   \
          //  Ident   Program
          //         /   |   \
          //     Instr Instr Instr
          //
          // Where Ident holds the identifier of the program return value.

          return Seq << _(Ident)->clone() << (prog << topExpressions);
        },

        /**
         * Compile TopExpression.
         */
        T(Compile) << T(Ident)[Ident] * (T(TopExpr) << T(Expr, Let)[Expr]) >>
          [](Match& _) -> Node {
          // FIXME: Debug print
          std::cout << "TopExpr" << std::endl;

          return (Compile << _(Ident) << _(Expr));
        },

        /**
         * Let
         */
        T(Compile) << T(Ident)[Result] *
              (T(Let)[Let] << T(Ident)[Ident] *
                 (T(Type) << (T(ForAllTy)[Type])) * T(Expr)[Expr]) >>
          [context](Match& _) -> Node {
          // FIXME: Debug print
          std::cout << "Let" << std::endl;

          // TODO: Need to deal with ForAllTy. Until I figure out how, just
          // pretend it cannot be nested and ignore it.
          Node type = get_type(_(Type));

          Node llvmType = getLLVMType(type);
          if (llvmType == nullptr) {
            return err(_(Type), "let type not supported");
          }

          // TODO: Might need to handle functions differently.
          return Seq << (Instr
                         << (MemoryOp << (Alloca << _(Ident) << llvmType)))
                     << (Compile << _(Result) << _(Expr))
                     << (Instr
                         << (MemoryOp
                             << (Store << _(Result)->clone()
                                       << _(Ident)->clone())));
        },

        /**
         * Boolean
         */
        T(Compile) << T(Ident)[Ident] *
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

          return Action << (CreateConst << _(Ident) << llvmType << value);
        },

        /**
         * Integer
         */
        T(Compile) << T(Ident)[Ident] *
              (T(Expr) << (T(Type) << T(TInt)[Type]) * T(Int)[Int]) >>
          [](Match& _) -> Node {
          Node value = IRValue ^ node_val(_(Int));

          Node llvmType = getLLVMType(_(Type));
          assert(llvmType);

          return Action << (CreateConst << _(Ident) << llvmType << value);
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
        T(Compile) << T(Ident)[Ident] *
              (T(Expr) << (T(Type) << T(TInt)[Type]) *
                 (T(Add, Sub, Mul)[BinaryOp] << T(Expr)[Lhs] * T(Expr)[Rhs])) >>
          [](Match& _) -> Node {
          // FIXME: Debug print
          std::cout << "BinOp" << std::endl;

          Node op = NULL;
          if (_(BinaryOp) == Add) {
            op = Add;
          } else if (_(BinaryOp) == Sub) {
            op = Sub;
          } else if (_(BinaryOp) == Mul) {
            op = Mul;
          } else {
            return err(_(BinaryOp), "binary operation not supported");
          }
          assert(op);

          Node llvmType = getLLVMType(_(Type));
          assert(llvmType);

          Node lhsIdent = Ident ^ _(Lhs)->fresh();
          Node rhsIdent = Ident ^ _(Rhs)->fresh();

          // Instruction
          return Seq << (Compile << lhsIdent->clone() << _(Lhs))
                     << (Compile << rhsIdent->clone() << _(Rhs))
                     << (Instr
                         << (BinaryOp
                             << (op << _(Ident) << llvmType << lhsIdent
                                    << rhsIdent)));
        },

        /**
         * Comparison
         */
        T(Compile) << T(Ident)[Ident] *
              (T(Expr) << (T(Type) << T(TBool)) *
                 (T(Equals, LT)[Op] << T(Expr)[Lhs] * T(Expr)[Rhs])) >>
          [](Match& _) -> Node {
          if (get_type(_(Lhs))->type() != get_type(_(Rhs))->type()) {
            return err(_(Op), "comparison operands have different types");
          }

          Node type = get_type(_(Lhs));
          Node llvmType = getLLVMType(type);
          if (llvmType == nullptr) {
            return err(_(Lhs) / Type, "comparison type not supported");
          }

          Node op = NULL;
          if (_(Op) == Equals) {
            op = EQ;
          } else if (_(Op) == LT) {
            op = ULT;
          } else {
            return err(_(Op), "comparison operator not supported");
          }

          Node lhsIdent = Ident ^ _(Lhs)->fresh();
          Node rhsIdent = Ident ^ _(Rhs)->fresh();

          return Seq << (Compile << lhsIdent->clone() << _(Lhs))
                     << (Compile << rhsIdent->clone() << _(Rhs))
                     << (Instr
                         << (MiscOp
                             << (Icmp << _(Ident) << op << llvmType << lhsIdent
                                      << rhsIdent)));
        },

        /**
         * If-then-else
         */
        T(Compile) << T(Ident)[Ident] *
              (T(Expr)[Expr] << T(Type) *
                 (T(If) << T(Expr)[Cond] * T(Expr)[True] * T(Expr)[False])) >>
          [](Match& _) -> Node {
          // FIXME: Debug print
          std::cout << "If-then-else" << std::endl;

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
              (T(Expr) << (T(Type)) *
                 (T(FunCall) << (T(Expr)[Fun]) * T(Expr)[Param])) >>
          [](Match& _) -> Node {
          Node argIdent = Ident ^ _(Param)->fresh();
          Node funIdent = Ident ^ _(Fun)->fresh();

          // FIXME: debug print
          std::cout << "Compile - Function Call" << std::endl;

          return Seq << (Compile << argIdent->clone() << _(Param))
                     << (Compile << funIdent->clone() << _(Fun))
                     << (Instr
                         << (MiscOp
                             << (Call << _(Result) << funIdent
                                      << (ArgList << argIdent))));
        },

        /**
         * Closure Call
         */
        T(Compile) << T(Ident)[Result] *
              (T(Expr) << (T(Type)) *
                 (T(ClosureCall) << (T(Expr)[Fun]) * T(Expr)[Param])) >>
          [](Match& _) -> Node {
          // FIXME: This treats all functions as if they are closures,
          // not sure how to deal with print.
          Node argIdent = Ident ^ _(Param)->fresh();
          Node funIdent = Ident ^ _(Param)->fresh();

          // TODO: Need to create function type from the function's type
          Node funRetTypeToken = _(Fun) / Type / Type / Ty1;
          Node funArgTypeToken = _(Fun) / Type / Type / Ty2;
          Node retLLVMType = getLLVMType(funRetTypeToken);
          Node argLLVMType = getLLVMType(funArgTypeToken);

          std::string uniqueId = std::string(_(Fun)->fresh().view());

          Node closurePtr = Ident ^ "closPtr_" + uniqueId;
          Node closureTy = Ident ^ "ClosureTy";

          Node funSlotPtr = Ident ^ "funSlot_" + uniqueId;
          Node funPtr = Ident ^ "funPtr_" + uniqueId;
          Node funTy = Ident ^ "funType_" + uniqueId;

          Node envSlotPtr = Ident ^ "envSlot_" + uniqueId;
          Node envPtr = Ident ^ "envPtr_" + uniqueId;

          return Seq
            // Closure
            << (Compile << closurePtr << _(Fun))
            // Env
            << (Instr
                << (MemoryOp
                    << (GetElementPtr
                        << envSlotPtr << closureTy << closurePtr->clone()
                        << (OffsetList
                            << (Offset << Ti32 << (IRValue ^ "0"))
                            << (Offset << Ti32 << (IRValue ^ "0"))))))
            << (Instr
                << (MemoryOp
                    << (Load << envPtr << TPtr << envSlotPtr->clone())))
            // Fun
            << (Instr
                << (MemoryOp
                    << (GetElementPtr
                        << funSlotPtr << closureTy->clone()
                        << closurePtr->clone()
                        << (OffsetList
                            << (Offset << Ti32 << (IRValue ^ "0"))
                            << (Offset << Ti32 << (IRValue ^ "1"))))))
            << (Instr
                << (MemoryOp
                    << (Load << funPtr << TPtr << funSlotPtr->clone())))
            // Argument
            << (Compile << argIdent << _(Param))
            // TODO: Insert function type here, so it is callable.
            << (Action
                << (CreateFunType << funTy << retLLVMType
                                  << (IRTypeList << TPtr << argLLVMType)))
            // Function call
            << (Instr
                << (MiscOp
                    << (CallOpaque
                        << _(Result) << funTy->clone() << funPtr->clone()
                        << (ArgList << envPtr->clone() << argIdent->clone()))));
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
        T(Compile)
            << (T(IRFun)[IRFun]
                << (T(Ident)[Ident] * T(Type)[Type] * T(ParamList)[ParamList] *
                    T(Env)[Env] * (T(Body)[Body] << (Any++)[Expr]))) >>
          [](Match& _) -> Node {
          // FIXME debug print
          std::cout << "compiling function: " << node_val(_(IRFun))
                    << std::endl;

          // TODO: Support polymorphism (Tvar)
          Node type = _(Type) / Type;
          Node paramType = getLLVMType(type / Ty1);
          Node returnType = getLLVMType(type / Ty2);
          assert(paramType);
          assert(returnType);

          Node funType = TypeArrow << paramType << returnType;

          // TODO: Deal with functions identifier, i.e. `f` in `fun f (x) is
          // x;;`
          std::string uniqueId = std::string(_(IRFun)->fresh().view());
          Node entryPoint = Label ^ ("entry_" + uniqueId);
          Node returnId = Ident ^ ("ret_" + uniqueId);

          // TODO: Add free variables to environment

          if ("main" == node_val(_(IRFun))) {
            return Seq
              << ((IRFun ^ node_val(_(IRFun)))
                  << funType << (Compile << _(ParamList))
                  << (Body
                      << entryPoint
                      // TODO: Instructions for loading from environment here
                      // FIXME: We need the free variable list to create
                      // those instructions :/
                      // main() may contain multiple expressions.
                      << (Compile << (TODO << _[Expr]))
                      // main() expected to return 0 for program success.
                      << (Action
                          << (CreateConst << returnId->clone() << Ti32
                                          << (IRValue ^ "0")))
                      << (Instr
                          << (TerminatorOp << (Ret << returnId->clone())))));
          } else {
            return Seq
              << ((IRFun ^ node_val(_(IRFun)))
                  << funType << (Compile << _(ParamList))
                  << (Body
                      << entryPoint
                      // TODO: Instructions for loading from environment here
                      // FIXME: We need the free variable list to create
                      // those instructions :/

                      // Lambdas can only contain a single expression.
                      << (Compile << returnId << _(Expr))
                      // MiniML has implicit return so must insert it here.
                      << (Instr
                          << (TerminatorOp << (Ret << returnId->clone())))));
          }
        },

        /**
         * Environment
         */
        T(Compile) << T(Env)[Env] >> [](Match& _) -> Node {
          // FIXME: debug
          std::cout << "Environment" << std::endl;

          if (_(Env)->size() == 0) {
            // Remove unnecessary environments
            return {};
          }

          Node env = _(Env);
          Node name = Ident ^ node_val(env);

          std::cout << env->size() << std::endl;

          for (size_t i = 0; i < env->size(); i++) {
            Node type = env->at(i);
            env->replace_at(i, (Compile << type));
          }

          auto typesToCompile = *_(Env);

          return Action
            << (CreateStructType << name << (IRTypeList << typesToCompile));
        },

        /**
         * Closure
         */
        T(Compile)
            << (T(Ident)[Result] *
                (T(Expr)
                 << (T(Type) *
                     (T(CreateClosure)[CreateClosure]
                      << (T(Ident)[Fun] * T(Env)[Env] *
                          (T(FreeVarList)[FreeVarList])))))) >>
          [](Match& _) -> Node {
          // FIXME: debug print
          std::cout << "compiling closure" << std::endl;

          Node funToCall = Ident ^ "malloc";

          // FIXME: Assumes a ptr is i64 = 8 Bytes.
          size_t closureByteCount = 20;
          Node closureBytesValue = IRValue ^ std::to_string(closureByteCount);
          Node closureBytes = Ident ^ _(CreateClosure)->fresh();
          // Node closurePtr = Ident ^ _(Closure)->fresh();
          Node closurePtr = _(Result);
          Node closureTy = Ident ^ "ClosureTy";

          std::string uniqueId = std::string(_(CreateClosure)->fresh().view());

          Node envSlotPtr = Ident ^ "envSlot_" + uniqueId;
          Node envPtr = Ident ^ "envPtr_" + uniqueId;

          Node funSlotPtr = Ident ^ "funSlot_" + uniqueId;
          Node funPtr = Ident ^ "funPtr_" + uniqueId;

          // TODO: Handle free var and none freevar separately
          if (_(FreeVarList)->size() == 0) {
            // CLOSURE WITHOUT ENVIRONMENT.
            return Seq
              // TODO: Malloc Closure.
              << (Action
                  << (CreateConst << closureBytes << Ti64 << closureBytesValue))
              << (Instr
                  << (MiscOp
                      << (Call << closurePtr << funToCall->clone()
                               << (ArgList << closureBytes->clone()))))

              // GEP function slot in closure.
              << (Instr
                  << (MemoryOp
                      << (GetElementPtr
                          << funSlotPtr << closureTy->clone()
                          << closurePtr->clone()
                          << (OffsetList
                              << (Offset << Ti32 << (IRValue ^ "0"))
                              << (Offset << Ti32 << (IRValue ^ "1"))))))

              // Store function ptr in closure.
              << (Action << (GetFunction << funPtr << _(Fun)))
              << (Instr
                  << (MemoryOp
                      << (Store << funPtr->clone() << funSlotPtr->clone())));
          } else {
            // CLOSURE WITH ENVIRONMENT.

            // FIXME: dynamic calc of malloc size
            // FIXME: Assumes single integer
            // FIXME: Not sure how to do it otherwise
            for (size_t i = 0; i < _(FreeVarList)->size(); i++) {
              // DO nothing
            }
            size_t envByteCount = 4;

            Node envBytesValue = IRValue ^ std::to_string(envByteCount);
            Node envBytes = Ident ^ _(CreateClosure)->fresh();
            Node envTy = Ident ^ node_val(_(Env));

            return Seq
              // TODO: Malloc environment
              << (Action << (CreateConst << envBytes << Ti64 << envBytesValue))
              << (Instr
                  << (MiscOp
                      << (Call << envPtr << funToCall
                               << (ArgList << envBytes->clone()))))

              // TODO: ForEach FreeVar, do:
              // TODO: Load freeVar.
              // TODO: GEP freeVar slot in envPtr
              // TODO: Store freeVar.
              // TODO: This has to be done in a separate compile node :/
              // << (Compile << (Load and Store FreeVars to environment ptr))
              // _(Env) = ident for struct type
              << (Compile << envTy->clone() << envPtr->clone() << FreeVarList)

              // TODO: Malloc Closure.
              << (Action
                  << (CreateConst << closureBytes << Ti64 << closureBytesValue))
              << (Instr
                  << (MiscOp
                      << (Call << closurePtr << funToCall->clone()
                               << (ArgList << closureBytes->clone()))))
              // GEP env slot in closure.
              << (Instr
                  << (MemoryOp
                      << (GetElementPtr
                          << envSlotPtr << closureTy << closurePtr->clone()
                          << (OffsetList
                              << (Offset << Ti32 << (IRValue ^ "0"))
                              << (Offset << Ti32 << (IRValue ^ "0"))))))

              // Store envptr in closure.
              << (Instr
                  << (MemoryOp
                      << (Store << envPtr->clone() << envSlotPtr->clone())))

              // GEP function slot in closure.
              << (Instr
                  << (MemoryOp
                      << (GetElementPtr
                          << funSlotPtr << closureTy->clone()
                          << closurePtr->clone()
                          << (OffsetList
                              << (Offset << Ti32 << (IRValue ^ "0"))
                              << (Offset << Ti32 << (IRValue ^ "1"))))))

              // Store function ptr in closure.
              << (Action << (GetFunction << funPtr << _(Fun)))
              << (Instr
                  << (MemoryOp
                      << (Store << funPtr->clone() << funSlotPtr->clone())));
          }
        },

        /**
         * Type
         */
        T(Compile) << T(Type)[Type] >> [](Match& _) -> Node {
          Node llvmType = getLLVMType(_(Type) / Type);
          if (llvmType == nullptr) {
            return err(
              _(Type) / Type, "Cannot convert to equivalent LLVM IR type");
          }

          return llvmType;
        },

        /**
         * ParamList
         */
        T(Compile) << T(ParamList)[ParamList] >> [](Match& _) -> Node {
          Node parent = _(ParamList);

          for (size_t i = 0; i < parent->size(); i++) {
            Node child = parent->at(i);
            parent->replace_at(i, (Compile << child));
          }

          return parent;
        },

        /**
         * Parameter
         */
        T(Compile) << (T(Param) << T(Ident)[Ident] * T(Type)[Type]) >>
          [](Match& _) -> Node {
          Node llvmType = getLLVMType(_(Type) / Type);
          assert(llvmType);

          return Param << _(Ident) << llvmType;
        },

        /**
         * FreeVarList
         */
        T(Compile) << T(Ident)[Env] * T(Ident)[TPtr] *
              T(FreeVarList)[FreeVarList] >>
          [](Match& _) -> Node {
          // FIXME: debug print
          std::cout << "compiling freevarlist" << std::endl;
          Node envPtr = _(TPtr);
          Node envTy = _(Env);

          Node seq = Seq;
          for (size_t i = 0; i < _(FreeVarList)->size(); i++) {
            Node freeVar = _(FreeVarList)->at(i);
            Node ident = freeVar / Ident;
            Node type = freeVar / Type;

            Node tmp = Ident ^ _(FreeVarList)->fresh();
            Node freeVarSlot = Ident ^ _(FreeVarList)->fresh();

            // Load FV from enclosing scope
            seq << (Instr << (MemoryOp << (Load << tmp << type << ident)));
            // GEP FV slot in environment (need env_type, env_ptr and i)
            seq
              << (Instr
                  << (MemoryOp
                      << (GetElementPtr
                          << freeVarSlot << envTy->clone() << envPtr->clone()
                          << (OffsetList
                              << (Offset << Ti32 << (IRValue ^ "0"))
                              << (Offset << Ti32
                                         << (IRValue ^ std::to_string(i)))))));
            // Store FV in env
            seq
              << (Instr << (MemoryOp << (Store << tmp << type << freeVarSlot)));
          }

          return seq;
        },

        // Propagate Compile onto several nodes.
        T(Compile) << T(TODO)[TODO] >> [](Match& _) -> Node {
          for (size_t i = 0; i < _(TODO)->size(); i++) {
            Node child = _(TODO)->at(i);
            Node ident = Ident ^ _(TODO)->fresh();
            _(TODO)->replace_at(i, Compile << ident << child);
          }

          auto children = *_(TODO);

          return Seq << children;
        },
      }};
  }
}
