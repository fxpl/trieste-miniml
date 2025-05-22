#include "../../miniml-lang.hh"
#include "../internal.hh"
#include "../llvm_utils.hh"
#include "../utils.hh"
#include "trieste/token.h"

namespace miniml {

  using namespace trieste;

  /**
   * @brief
   * Reverses the ordering of blocks in functions.
   *
   * @example
   *            Fun
   *           /   \
   *       BlockA  BlockB
   *      /  |  \
   * Label Instr Instr
   * ==>
   *      Fun
   *     /   \
   * BlockB  BlockA
   *        /  |  \
   *   Label Instr Instr
   *
   */
  PassDef reverse_blocks() {
    return {
      "reverse_blocks",
      LLVMIRBlockify::wf,
      (dir::topdown | dir::once),
      {
        /**
         * Reverse order of basic blocks in a function.
         */
        In(IRFun) * (T(Body)[Body] << T(Block)++) >> [](Match& _) -> Node {
          auto children = *_(Body);
          std::reverse(children.begin(), children.end());

          _(Body)->erase(_(Body)->begin(), _(Body)->end());

          return _(Body) << children;
        },

      }};
  }
}
