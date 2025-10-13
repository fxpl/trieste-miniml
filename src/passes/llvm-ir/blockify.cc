#include "../../llvm-lang.hh"
#include "../../miniml-lang.hh"
#include "../utils.hh"
#include "trieste/token.h"

namespace miniml {

  using namespace trieste;

  /**
   * Transforms the body of functions from a linear sequence of instructions
   * into an hierarchical structure consisting of blocks.
   */
  PassDef blockify() {
    return {
      "blockify",
      llvmir::wf,
      (dir::topdown | dir::once),
      {
        In(llvmir::IRFun) *
            (T(llvmir::Body)[Body] << (Start * !T(llvmir::Block))) >>
          [](Match& _) -> Node {
          Node blockifiedBody = llvmir::Body;

          Node block = nullptr;
          Node statements = nullptr;
          for (Node child : *_(Body)) {
            if (child == llvmir::Label) {
              block = llvmir::Block ^ node_val(child);
              block->push_back(child);
              statements = llvmir::Statements;
            } else if (block && child == llvmir::TerminatorOp) {
              block->push_back(statements);
              block->push_back(llvmir::Terminator << child);
              blockifiedBody->push_back(block);

              block = nullptr;
              statements = nullptr;
            } else if (block) {
              statements->push_back(child);
            } else {
              return err(child, "Instruction or terminator outside of block");
            }
          }

          return blockifiedBody;
        },
      }};
  }
}
