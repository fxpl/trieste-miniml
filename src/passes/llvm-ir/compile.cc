#include "../../miniml-lang.hh"
#include "../internal.hh"
#include "../llvm_utils.hh"
#include "../utils.hh"
#include "trieste/token.h"

namespace miniml {

  using namespace trieste;

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
    return {
      "compile",
      LLVMIRCompilation::wf,
      (dir::topdown | dir::once),
      {
        /**
         * Add compile node and assign tmp/register.
         */
        In(Top) * T(Program)[Program] >> [](Match& _) -> Node {
          Node result = Ident ^ _(Program)->fresh();

          return Reapply << (Compile << result << _(Program));
        },

        /**
         * Compile program and add compile nodes to its children.
         */
        T(Compile) << T(Ident)[Ident] * T(Program)[Program] >>
          [](Match& _) -> Node {
          auto prog = _(Program);

          Node reapply = Reapply;

          // Generate identifiers for each of the program's top expressions.
          for (size_t i = 0; i < prog->size() - 1; i++) {
            Node ident = Ident ^ prog->fresh();
            Node topexpr = prog->at(i);

            reapply << (Compile << ident << topexpr);
          }

          // The last topexpression is bound to the identifier of the program.
          Node topexpr = prog->at(prog->size() - 1);
          reapply << (Compile << _(Ident) << topexpr);

          // TODO: Is this still a good idea?
          //
          //       Top
          //      /   \
          //  Ident   Program
          //         /   |   \
          //     Instr Instr Instr
          //
          // Where Ident holds the identifier of the program return value.

          return reapply;
        },

        /**
         * Compile TopExpression.
         */
        T(Compile) << T(Ident)[Ident] * (T(TopExpr) << T(Expr, Let)[Expr]) >>
          [](Match& _) -> Node {
          return Reapply << (Compile << _(Ident) << _(Expr));
        },

        /**
         * Let
         */
        T(Compile) << T(Ident)[Result] *
              (T(Let)[Let] << T(Ident)[Ident] *
                 (T(Type) << (T(ForAllTy)[Type])) * T(Expr)[Expr]) >>
          [](Match& _) -> Node {
          // TODO: Let now stores it on the stack and then loads it into a
          // register. Should probably just store it and use another token as
          // Identifier so it is translated into a load before use.

          Node tmpPtr = Ident ^ _(Let)->fresh();

          // TODO: Need to deal with ForAllTy. Until I figure out how, just
          // pretend it cannot be nested and ignore it.
          Node type = get_type(_(Type));

          Node llvmType = getLLVMType(type);
          if (llvmType == nullptr) {
            return err(_(Type), "let type not supported");
          }

          // Bind expr to both Ident and Dst
          return Reapply
            << (Compile << _(Ident) << _(Expr))
            << (Lift << Top
                     << (Meta << (RegCpy << _(Result) << _(Ident)->clone())));
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

          return Meta << (RegMap << _(Ident) << llvmType << value);
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

          return Meta << (RegMap << _(Ident) << llvmType << value);
        },

        /**
         * Identifier
         */
        T(Compile) << T(Ident)[Dst] * (T(Expr) << T(Type) * T(Ident)[Src]) >>
          [](Match& _) -> Node {
          // FIXME: This is required to handle programs that end in :
          // let x = 3;;
          // x;;
          return Meta << (RegCpy << _(Dst) << _(Src));
        },

        /**
         * Binary operation.
         */
        T(Compile) << T(Ident)[Ident] *
              (T(Expr) << (T(Type) << T(TInt)[Type]) *
                 (T(Add, Sub, Mul)[BinaryOp] << T(Expr)[Lhs] * T(Expr)[Rhs])) >>
          [](Match& _) -> Node {
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
          return Reapply << (Compile << lhsIdent->clone() << _(Lhs))
                         << (Compile << rhsIdent->clone() << _(Rhs))
                         << (Lift << Top
                                  << (Instr
                                      << (BinaryOp
                                          << (op << _(Ident) << llvmType
                                                 << lhsIdent << rhsIdent))));
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

          return Reapply << (Compile << lhsIdent->clone() << _(Lhs))
                         << (Compile << rhsIdent->clone() << _(Rhs))
                         << (Lift << Top
                                  << (Instr
                                      << (MiscOp
                                          << (Icmp << _(Ident) << op << llvmType
                                                   << lhsIdent << rhsIdent))));
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
          Node thenLabel = Ident ^ ("then" + ifId);
          Node thenEndLabel = Ident ^ ("thenEnd" + ifId);
          Node elseLabel = Ident ^ ("else" + ifId);
          Node elseEndLabel = Ident ^ ("elseEnd" + ifId);
          Node ifEndLabel = Ident ^ ("ifEnd" + ifId);

          return Reapply
            << (Compile << condId << _(Cond))
            // Generate blocks/labels
            << (Lift << Top << (Meta << (BlockMap << thenLabel)))
            << (Lift << Top << (Meta << (BlockMap << thenEndLabel)))
            << (Lift << Top << (Meta << (BlockMap << elseLabel)))
            << (Lift << Top << (Meta << (BlockMap << elseEndLabel)))
            << (Lift << Top << (Meta << (BlockMap << ifEndLabel)))
            // If
            << (Lift << Top
                     << (Instr
                         << (TerminatorOp
                             << (Branch << condId->clone() << thenLabel->clone()
                                        << elseLabel->clone()))))
            // Then
            << (Lift << Top << (Label << thenLabel->clone()))
            << (Compile << ifTrueId << _(True))
            << (Lift << Top
                     << (Instr
                         << (TerminatorOp << (Jump << thenEndLabel->clone()))))
            // "Landing" block so Phi unaffected by branching in Then/Else expr.
            << (Lift << Top << (Label << thenEndLabel->clone()))
            << (Lift << Top
                     << (Instr
                         << (TerminatorOp << (Jump << ifEndLabel->clone()))))
            // Else
            << (Lift << Top << (Label << elseLabel->clone()))
            << (Compile << ifFalseId << _(False))
            << (Lift << Top
                     << (Instr
                         << (TerminatorOp << (Jump << elseEndLabel->clone()))))
            // "Landing" block so Phi unaffected by branching in Then/Else expr.
            << (Lift << Top << (Label << elseEndLabel->clone()))
            << (Lift << Top
                     << (Instr
                         << (TerminatorOp << (Jump << ifEndLabel->clone()))))
            // ifEnd
            << (Lift << Top << (Label << ifEndLabel->clone()))
            << (Lift << Top
                     << (Instr
                         << (MiscOp
                             << (Phi
                                 << _(Ident) << llvmType
                                 << (Predecessor
                                     << (Prev << ifTrueId->clone()
                                              << thenEndLabel->clone())
                                     << (Prev << ifFalseId->clone()
                                              << elseEndLabel->clone()))))));
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

          return Reapply << (Compile << argIdent->clone() << _(Param))
                         << (Compile << funIdent->clone() << _(Fun))
                         << (Lift << Top
                                  << (Instr
                                      << (MiscOp
                                          << (Call << _(Ident) << funIdent
                                                   << argIdent))));
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

          return Lift << Top << (Meta << (FuncMap << _(Ident) << funcName));
        },

        /**
         * Fun
         */
        T(Compile) << T(Ident)[Ident] *
              (T(Expr) << T(Type) *
                 (T(Fun)
                  << (T(FunDef) << T(Ident)[Fun] * T(Type)[Type] *
                        T(Param)[Param] * T(Expr)[Expr]))) >>
          [](Match& _) -> Node {
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
          // let id = fun f x is x;;
          // Both ´id´(Ident) and ´f´(Fun) point to same function.
          // But f is only inside the scope of the function.
          // Can we use Trieste symboltable maybe?
          Node f = _(Fun);

          std::string funStr = node_val(_(Ident));

          Node funBodyLabel = Ident ^ ("funBody_" + funStr);

          Node originBlock = Ident ^ ("prevBlock_" + funStr);
          Node returnId = Ident ^ ("retVal_" + funStr);

          // -- need to bind the arguments of the function
          // -- here, any free variables?!

          return Reapply
            // Create function body block.
            // << (Lift << Top << (Meta << (BlockMap << funBodyLabel)))
            // Remember block where function was declared.
            << (Lift << Top << (Meta << (BlockCpy << originBlock)))
            // Tell code generator to create a function: its type, name, param.
            << (Lift << Top
                     << (FunDef << _(Ident) << funType << (_(Param) / Ident)))
            // Bind function to its internal name.
            << (Lift << Top << (Meta << (FuncMap << f << _(Ident)->clone())))
            // Set builder insertion point to function body.
            // << (Lift << Top << (Label << funBodyLabel->clone()))
            // TODO: Need to bind the block to the function.
            << (Compile << returnId << _(Expr))
            // MiniML has implicit return so must insert it here.
            << (Lift << Top
                     << (Instr << (TerminatorOp << (Ret << returnId->clone()))))
            // TODO: Pop current function.
            //       (So any future blocks does not belong to this function)
            // Reset IR builder insertion point.
            << (Lift << Top << (Label << originBlock->clone()));
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
