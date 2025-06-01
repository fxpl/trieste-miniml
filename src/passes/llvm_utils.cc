#include "./llvm_utils.hh"

#include "../llvm-lang.hh"

namespace llvmir {
  using namespace trieste;
  using namespace llvm;

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

  std::string node_val(Node node) {
    std::string text(node->location().view());
    return text;
  }
}
