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
      // TODO: Use create WF
      LLVMIRBlockify::wf,
      (dir::topdown | dir::once),
      {
        /**
         * Reverse order of basic blocks in a function.
         */
        T(IRFun)[IRFun] << T(Block)++ >> [](Match& _) -> Node {
          // FIXME: debug print
          std::cout << "Reverse" << std::endl;

          auto children = *_(IRFun);
          std::reverse(children.begin(), children.end());

          _(IRFun)->erase(_(IRFun)->begin(), _(IRFun)->end());

          return _(IRFun) << children;
        },

      }};
  }
}
