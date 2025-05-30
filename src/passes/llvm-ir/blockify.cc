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
   * Transforms the program from a linear stream of instructions
   * into a program->function->block->instruction structure:
   *
   *         Top
   *        /   \
   *    Ident   Program
   *           /   |   \
   *       Func  Func  Func
   *               |
   *             Body
   *            /    \
   *         Block  Block
   *        /  |  \
   *   Label Instr Instr
   *
   */
  PassDef blockify() {
    return {
      "blockify",
      LLVMIRBlockify::wf,
      (dir::topdown),
      {
        /**
         * Initialize compile cursor for blockify pass.
         */
        In(IRFun) * (T(Body)[Body] << (Start * !T(Block, Compile))) >>
          [](Match& _) -> Node {
          auto children = *_(Body);

          return Body << (Compile << children);
        },

        /**
         * Create new block in current function.
         */
        T(Compile)[Compile] << Start * T(Label)[Label] >> [](Match& _) -> Node {
          Node block = Block ^ node_val(_(Label));
          Node label = pop_front(_(Compile));

          return Lift << Body << (block << label << _(Compile));
        },

        /**
         * Add instruction to current block.
         */
        T(Compile)[Compile] << Start * !T(Label) >> [](Match& _) -> Node {
          Node node = pop_front(_(Compile));

          return Seq << (Lift << Block << node) << _(Compile);
        },

        /**
         * Blockification completed
         */
        T(Compile) << Start * End >> [](Match& _) -> Node { return {}; },
      }};
  }
}
