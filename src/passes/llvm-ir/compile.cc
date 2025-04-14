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
         * Compile Let.
         */
        T(Compile) << T(Ident)[Ident] *
              (T(Let)[Let] << T(Ident)[Dst] * T(Type)[Type] * T(Expr)[Expr]) >>
          [](Match& _) -> Node {
          // TODO: Can't I just use registers for everything and let LLVM
          // optimize?
          //       Problem is both Ident and Dst need to hold the same register
          //       This will probably be a problem when assigning functions to
          //       names. It needs to be recompiled as a function declaration I
          //       guess.

          Node tmpPtr = Ident ^ _(Let)->fresh();

          // TODO: Hardcoding type as TInt since unsure how to deal with
          // ForAllTy
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
         * Compile single integer.
         */
        T(Compile) << T(Ident)[Ident] *
              (T(Expr) << (T(Type)[Type] << T(TInt)) * T(Int)[Int]) >>
          [](Match& _) -> Node {
          // Enable storing integers to register by adding with 0.
          return Instr
            << (BinaryOp
                << (Add << _(Ident) << _(Type) << _(Int) << (Int ^ "0")));
        },

        /**
         * Compile single identifier.
         */
        T(Compile) << T(Ident)[Dst] * (T(Expr) << T(Type) * T(Ident)[Src]) >>
          [](Match& _) -> Node {
          // FIXME: This is required to handle programs that end in :
          // let x = 3;;
          // x;;
          return Meta << (RegCpy << _(Dst) << _(Src));
        },

        /**
         * Compile binary operation.
         */
        T(Compile) << T(Ident)[Ident] *
              (T(Expr) << (T(Type)[Type] << T(TInt)) *
                 (T(Add, Sub, Mul)[BinaryOp] << T(Expr)[Lhs] * T(Expr)[Rhs])) >>
          [](Match& _) -> Node {
          Token op;
          if (_(BinaryOp)->type() == Add) {
            op = Add;
          } else if (_(BinaryOp)->type() == Sub) {
            op = Sub;
          } else if (_(BinaryOp)->type() == Mul) {
            op = Mul;
          } else {
            // TODO: Return error
          }

          Node reapply = Reapply;

          // Avoids compilation of identifier expressions.
          // TODO: I am worried that this needs to be implemented in many rules
          Node LhsIdent;
          if (_(Lhs) / Expr == Ident) {
            LhsIdent = _(Lhs) / Expr;
          } else {
            LhsIdent = Ident ^ _(Lhs)->fresh();
            reapply << (Compile << LhsIdent->clone() << _(Lhs));
          }

          Node RhsIdent;
          if (_(Rhs) / Expr == Ident) {
            RhsIdent = _(Rhs) / Expr;
          } else {
            RhsIdent = Ident ^ _(Rhs)->fresh();
            reapply << (Compile << RhsIdent->clone() << _(Rhs));
          }

          // Instruction
          reapply
            << (Lift << Top
                     << (Instr
                         << (BinaryOp
                             << (op << _(Ident) << _(Type) << LhsIdent
                                    << RhsIdent))));
          return reapply;
        },

        /**
         * Apply
         */
        T(Compile) << T(Ident)[Ident] *
              (T(Expr) << (T(Type) << T(TInt)[Type]) *
                 (T(App) << (T(Expr)[Fun]) * T(Expr)[Param])) >>
          [](Match& _) -> Node {
          Node argIdent = Ident ^ _(Param)->fresh();
          Node funIdent = Ident ^ _(Fun)->fresh();

          return Reapply << (Compile << argIdent->clone() << _(Param))
                         << (Compile << funIdent->clone() << _(Fun))
                         << (Lift << Top
                                  << (Instr
                                      << (MiscOp
                                          << (FunCall << _(Ident) << funIdent
                                                      << argIdent))));
        },

        /**
         * Print
         */
        T(Compile) << T(Ident)[Ident] *
              (T(Expr) << (T(Type) << T(TypeArrow)[TypeArrow]) * T(Print)) >>
          [](Match& _) -> Node {

          Node funcName = NULL;
          if ((_(TypeArrow) / Ty1 == TInt) && (_(TypeArrow) / Ty2 == TInt)) {
            funcName = Ident ^ "printInt";
          } // else if ()
          // TODO: Support print for other types.

          if (!funcName) {
            return err(_(TypeArrow), "print function with this type not found");
          }

          return Lift << Top << (Meta << (FuncMap << _(Ident) << funcName));
        },
      }};
  }
}
