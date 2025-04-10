#include "../../miniml-lang.hh"
#include "../internal.hh"
#include "../utils.hh"
#include "trieste/token.h"

namespace miniml {

  using namespace trieste;

  /**
   * @brief

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
      LLVMIRGeneration::wf,
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
         * Add evaluate expression and store it in tmp/register.
         */
        T(Compile) << T(Ident)[Ident] * (T(TopExpr) << T(Expr)[Expr]) >>
          [](Match& _) -> Node {
          return Reapply << (Compile << _(Ident) << _(Expr));
        },

        /**
         * Compile expression that is a single integer.
         */
        T(Compile) << T(Ident)[Ident] *
              (T(Expr) << T(Type)[Type] * T(Int)[Int]) >>
          [](Match& _) -> Node {
          // Enable storing to register by adding with 0
          return Instr
            << (BinaryOp
                << (Add << _(Ident) << _(Type) << _(Int) << (Int ^ "0")));
        },

        /**
         * Compiles an addition by:
         * appending a binary Op instruction and inserting Compile nodes for its
         * child Exprs.
         */
        T(Compile) << T(Ident)[Ident] *
              (T(Expr) << T(Type)[Type] *
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

          Node LhsIdent = Ident ^ _(Lhs)->fresh();
          Node RhsIdent = Ident ^ _(Rhs)->fresh();

          return Reapply << (Compile << LhsIdent->clone() << _(Lhs))
                         << (Compile << RhsIdent->clone() << _(Rhs))
                         << (Lift << Top
                                  << (Instr
                                      << (BinaryOp
                                          << (op << _(Ident) << _(Type)
                                                 << LhsIdent << RhsIdent))));
        },
      }};
  }
}
