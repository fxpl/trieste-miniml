#include "../../miniml-lang.hh"
#include "../internal.hh"
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
        T(Compile) << T(Ident)[Ident] *
              (T(Let)[Let] << T(Ident)[Dst] * T(Type)[Type] * T(Expr)[Expr]) >>
          [](Match& _) -> Node {
          // TODO: Let now stores it on the stack and then loads it into a
          // register. Should probably just store it and use another token as
          // Identifier so it is translated into a load before use.

          Node tmpPtr = Ident ^ _(Let)->fresh();

          // TODO: Need to deal with ForAllTy. Until I figure out how, use
          // hardcoded type.
          // FIXME: Let cannot be used for booleans.
          Node hardcodedType = (Type << TInt);

          return Reapply
            << (Compile << _(Dst) << _(Expr))
            // Then copy it into _(Ident) so [[_]]ident still true.
            << (Lift << Top
                     << (Instr
                         << (MemoryOp << (Alloca << tmpPtr << hardcodedType))))
            << (Lift << Top
                     << (Instr
                         << (MemoryOp
                             << (Store << _(Dst)->clone() << tmpPtr->clone()))))
            << (Lift << Top
                     << (Instr
                         << (MemoryOp
                             << (Load << _(Ident) << hardcodedType->clone()
                                      << tmpPtr->clone()))));
        },

        /**
         * Boolean
         */
        T(Compile) << T(Ident)[Ident] *
              (T(Expr) << (T(Type) << T(TBool)) * T(True, False)[IRValue]) >>
          [](Match& _) -> Node {
          Node value = NULL;
          if (_(IRValue) == True) {
            value = IRValue ^ "1";
          } else if (_(IRValue) == False) {
            value = IRValue ^ "0";
          }
          assert(value);

          return Meta << (RegMap << _(Ident) << Ti1 << value);
        },

        /**
         * Integer
         */
        T(Compile) << T(Ident)[Ident] *
              (T(Expr) << (T(Type) << T(TInt)) * T(Int)[Int]) >>
          [](Match& _) -> Node {
          Node value = IRValue ^ node_val(_(Int));

          return Meta << (RegMap << _(Ident) << Ti32 << value);
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
              (T(Expr) << (T(Type)[Type] << T(TInt)) *
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

          Node lhsIdent = Ident ^ _(Lhs)->fresh();
          Node rhsIdent = Ident ^ _(Rhs)->fresh();

          // Instruction
          return Reapply << (Compile << lhsIdent->clone() << _(Lhs))
                         << (Compile << rhsIdent->clone() << _(Rhs))
                         << (Lift << Top
                                  << (Instr
                                      << (BinaryOp
                                          << (op << _(Ident) << _(Type)
                                                 << lhsIdent << rhsIdent))));
        },

        /**
         * Comparison
         */
        T(Compile) << T(Ident)[Ident] *
              (T(Expr) << (T(Type) << T(TBool)) *
                 (T(Equals, LT)[Op] << T(Expr)[Lhs] * T(Expr)[Rhs])) >>
          [](Match& _) -> Node {
          if (
            (_(Lhs) / Type / Type)->type() != (_(Rhs) / Type / Type)->type()) {
            return err(_(Op), "comparison operands have different types");
          }

          Node type = get_type(_(Lhs));
          Node llvmType = NULL;
          if (type == TInt) {
            llvmType = Ti32;
          } else {
            return err(_(Lhs) / Type, "comparison type not supported");
          }

          Node op = NULL;
          if (_(Op) == Equals) {
            op = Eq;
          } else if (_(Op) == LT) {
            op = Ult;
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
         * Apply
         */
        T(Compile) << T(Ident)[Ident] *
              (T(Expr) << (T(Type) << T(TInt, TBool)[Type]) *
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
          std::cout << (_(TypeArrow) / Ty1)->type() << std::endl;
          
          if ((_(TypeArrow) / Ty1)->type() != (_(TypeArrow) / Ty2)->type()) {
            return err(_(TypeArrow), "print is an identity function");
          }

          if ((_(TypeArrow) / Ty1 == TInt) && (_(TypeArrow) / Ty2 == TInt)) {
            funcName = Ident ^ "printInt";
          } else if (
            (_(TypeArrow) / Ty1 == TBool) && (_(TypeArrow) / Ty2 == TBool)) {
            funcName = Ident ^ "printBool";
          }

          if (!funcName) {
            return err(_(TypeArrow), "print function with this type not found");
          }

          return Lift << Top << (Meta << (FuncMap << _(Ident) << funcName));
        },
      }};
  }
}
