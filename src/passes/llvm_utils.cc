#include "./llvm_utils.hh"

#include "../miniml-lang.hh"

namespace miniml {
  using namespace trieste;
  using namespace llvm;

  Node getLLVMType(Node type) {
    if (type == TInt) {
      return Ti32;
    } else if (type == TBool) {
      return Ti1;
    } else if (type == TypeArrow) {
      // TODO: Not sure how to handle this yet, so just return it
      return TPtr;
    } else if (type == TPtr) {
      return type;
    } else {
      return nullptr;
    }
  }

  llvm::Type* createLLVMType(std::shared_ptr<LLVMIRContext> ctx, Node type) {
    if (type == Ti1) {
      return ctx->builder.getInt1Ty();
    } else if (type == Ti32) {
      return ctx->builder.getInt32Ty();
    } else if (type == Ti64) {
      return ctx->builder.getInt64Ty();
    } else if (type == TPtr) {
      return ctx->builder.getPtrTy();
    } else {
      return nullptr;
    }
  }

  Node pop_front(Node node) {
    // FIXME: Hopefully there is a better way to pop first child.
    // FIXME: Since children are stored in a vector, pop-ing from front is
    // Oh(n).
    if (node->empty()) {
      return {};
    }

    // Keeps reference alive.
    Node child = node->front()->clone();
    node->erase(node->begin(), node->begin() + 1);

    return child;
  }
}
