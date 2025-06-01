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
      (dir::topdown | dir::once),
      {
        In(llvmir::IRFun) *
            (T(llvmir::Body)[Body] << (Start * !T(llvmir::Block))) >>
          [](Match& _) -> Node {
          Node blockifiedBody = llvmir::Body;

          Node currentBlock = nullptr;
          for (Node child : *_(Body)) {
            if (child == llvmir::Label) {
              currentBlock = llvmir::Block ^ node_val(child);
              currentBlock->push_back(child);
            } else if (child->front() == llvmir::TerminatorOp) {
              currentBlock->push_back(child);
              blockifiedBody->push_back(currentBlock);
              currentBlock = nullptr;
            } else {
              currentBlock->push_back(child);
            }
          }

          return blockifiedBody;
        },
      }};
  }
}
