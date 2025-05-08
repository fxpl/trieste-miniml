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
   * Transform the program from a linear stream of instructions
   * into a program->function->block->instruction structure:
   *
   *         Top
   *        /   \
   *    Ident   Program
   *           /   |   \
   *       Func  Func  Func
   *            /    \
   *         Block  Block
   *        /  |  \
   *   Label Instr Instr
   *
   * Once pass designed to iterate over each node in the program left to right.
   *
   */
  PassDef blockify() {
    return {
      "blockify",
      // TODO: Use create WF
      LLVMIRBlockify::wf,
      (dir::topdown),
      {
        /**
         * Initialize compile cursor for blockify pass.
         */
        In(Program) * T(IRFun)[IRFun] << (Start * !T(Block, Compile)) >>
          [](Match& _) -> Node {
          // FIXME: Debug print
          std::cout << "Blockify" << std::endl;

          // FIXME: Lifts blocks in reverse order, but instruction order seems
          // correct

          auto children = *_(IRFun);
          _(IRFun)->erase(_(IRFun)->begin(), _(IRFun)->end());

          return _(IRFun) << (Compile << children);
        },

        /**
         * Create new block and lift to closest function.
         */
        T(Compile)[Compile] << Start * T(Label)[Label] >> [](Match& _) -> Node {
          // FIXME: It's more Trieste-like to bind a matched forest of subtrees:
          // Example, this matches everything between two labels:
          // (T(Label) * (Any * --T(Label))++ * Any)[Block]
          // Should be usable to neatly split children of compile node.
          // Note: Needs _[Block] to deref. _(Block) derefs first child.
          Node block = Block ^ node_val(_(Label));
          Node label = pop_front(_(Compile));

          return Reapply << (Lift << IRFun << (block << label << _(Compile)));
        },

        /**
         * Add to current block.
         */
        T(Compile)[Compile] << Start * !T(Label) >> [](Match& _) -> Node {
          Node node = pop_front(_(Compile));

          return ((Lift << Block << node) << _(Compile));
        },

        /**
         * Blockification completed
         */
        T(Compile) << Start * End >> [](Match& _) -> Node {
          // Consume Compile node.
          return {};
        },
      }};
  }
}
