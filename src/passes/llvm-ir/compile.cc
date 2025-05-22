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
         * Apply
         */
        T(Compile) << T(Ident)[Ident] *
              (T(Expr) << (T(Type) << T(TInt, TBool)) *
                 (T(App) << (T(Expr)[Fun]) * T(Expr)[Param])) >>
          [](Match& _) -> Node {
          Node argIdent = Ident ^ _(Param)->fresh();
          Node funIdent = Ident ^ _(Fun)->fresh();

          return Seq << (Compile << argIdent->clone() << _(Param))
                     << (Compile << funIdent->clone() << _(Fun))
                     << (Instr
                         << (MiscOp
                             << (Call << _(Ident) << funIdent << argIdent)));
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

          // When compiling a function we need to
          // - create function type
          // TODO: How to deal with t_var? LLVM IR is strongly typed..
          // TODO: if type is TVar, then we need to create a function for each
          // type.
          // TODO: figure out how to identify which type to call when evaluating
          // the call.
          // FIXME: Just ignore TVar for now.
          Node type = _(Type) / Type;
          Node paramType = getLLVMType(type / Ty1);
          Node returnType = getLLVMType(type / Ty2);
          assert(paramType);
          assert(returnType);

          Node funType = TypeArrow << paramType << returnType;

          // TODO: Deal with function naming. example:
          // Both ´node_val(IRfun)' and ´f´(Fun) point to same function.
          // But f is only inside the scope of the function.
          std::string uniqueId = std::string(_(IRFun)->fresh().view());
          Node entryPoint = Label ^ ("entry_" + uniqueId);
          Node returnId = Ident ^ ("ret_" + uniqueId);

          // -- need to bind the arguments of the function
          // -- here, any free variables?!
          // FIXME: How to map free variables in fun to loads of closure?
          // Then a fun token should be lifted, containing the function body,
          // name and type. The lifted fun token is used by CodeGen pass to
          // declare function

          if ("main" == node_val(_(IRFun))) {
            return Seq
              << ((IRFun ^ node_val(_(IRFun)))
                  << funType << (Compile << _(ParamList))
                  << (Body
                      << entryPoint
                      // TODO: Instructions for loading from environment here
                      // FIXME: We need the free variable list to create
                      // those instructions :/
                      // FIXME: Main can contain multiple expressions!
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

          return Seq
            // TODO: Wait for closures to be done
            // Create function body block.
            // Remember block where function was declared.
            // Tell code generator to create a function: its type, name, param.
            << (Lift << Program
                     << ((IRFun ^ funStr)
                         << (FunDef << _(Ident) << funType
                                    << (_(Param) / Ident))
                         // Bind function to its internal name.
                         << (Meta << (FuncMap << f << _(Ident)->clone()))
                         // Set builder insertion point to function body.
                         // << (Lift << Top << (Label << funBodyLabel->clone()))
                         // TODO: Need to bind the block to the function.
                         << (Compile << returnId << _(Expr))
                         // MiniML has implicit return so must insert it here.
                         << (Instr
                             << (TerminatorOp << (Ret << returnId->clone())))));
          // TODO: Pop current function.
          //       (So any future blocks does not belong to this function)
          // Reset IR builder insertion point.
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
      }};
  }
}
