#include "../../llvm-lang.hh"
#include "trieste/token.h"

namespace llvmir {

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
      llvmir::wf,
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
