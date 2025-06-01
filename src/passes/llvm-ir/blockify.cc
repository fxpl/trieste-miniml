#include "../../llvm-lang.hh"
#include "../../miniml-lang.hh"
#include "../internal.hh"
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
      miniml::LLVMIRBlockify::wf,
      (dir::topdown),
      {
        /**
         * Initialize compile cursor for blockify pass.
         */
        In(llvmir::IRFun) *
            (T(llvmir::Body)[Body] << (Start * !T(llvmir::Block, Compile))) >>
          [](Match& _) -> Node {
          auto children = *_(Body);

          return llvmir::Body << (Compile << children);
        },

        /**
         * Create new block in current function.
         */
        T(Compile)[Compile] << Start * T(llvmir::Label)[llvmir::Label] >>
          [](Match& _) -> Node {
          Node block = llvmir::Block ^ node_val(_(llvmir::Label));
          Node label = pop_front(_(Compile));

          return Lift << llvmir::Body << (block << label << _(Compile));
        },

        /**
         * Add instruction to current block.
         */
        T(Compile)[Compile] << Start * !T(llvmir::Label) >>
          [](Match& _) -> Node {
          Node node = pop_front(_(Compile));

          return Seq << (Lift << llvmir::Block << node) << _(Compile);
        },

        /**
         * Blockification completed
         */
        T(Compile) << Start * End >> [](Match& _) -> Node { return {}; },
      }};
  }
}
