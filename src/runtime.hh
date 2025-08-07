#pragma once
#include "passes/llvm_utils.hh"

namespace llvmir {
  using namespace llvm;

  /**
   * Generates imports of external functions needed by the runtime.
   * @param context LLVM IR context
   */
  void genExternalFunctions(std::shared_ptr<LLVMIRContext> context);

  /**
   * Generates native and runtime functions in LLVM IR.
   * @param context LLVM IR context
   */
  void genRuntimeFunctions(std::shared_ptr<LLVMIRContext> context);

}
