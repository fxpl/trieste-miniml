#pragma once

#include "../llvm-lang.hh"

// LLVM code builder
/**
 * This is a workaround to prevent warnings from LLVM libraries
 * being treated as errors by CMake.
 */
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wunused-parameter"
#pragma clang diagnostic ignored "-Wshadow"

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/NoFolder.h> // Ignore constant folding for educational purposes
#pragma clang diagnostic pop

namespace llvmir {
  using namespace trieste;
  using namespace llvm;

  struct LLVMIRContext {
    // Holds core data structures, Type and constant value tables.
    llvm::LLVMContext llvm_context;
    // Generates LLVM IR instructions.
    // FIXME: NoFolder is used to prevent constant folding.
    llvm::IRBuilder<NoFolder> builder;
    // Contains generated instructions, local and global value tables.
    llvm::Module llvm_module;

    // Keeps track of generated BasicBlocks within program.
    std::unordered_map<std::string, llvm::BasicBlock*> basicBlocks;
    // Keeps track of generated LLVM types within program.
    std::unordered_map<std::string, llvm::Type*> types;

    // Keeps track of generated LLVM values within a function.
    std::unordered_map<std::string, llvm::Value*> registers;

    // Trieste's output file, if any.
    std::filesystem::path output_file;

    LLVMIRContext(std::string input_filepath, std::string output_filepath)
    : builder(llvm_context),
      llvm_module(input_filepath, llvm_context),
      output_file(std::filesystem::absolute(output_filepath)) {}

    ~LLVMIRContext() {}
  };

  /**
   * Creates a LLVM IR type from a miniML type token.
   * @param ctx reference to LLVM context.
   * @param type The miniML type token to convert.
   * @return The LLVM IR type.
   */
  llvm::Type* createLLVMType(std::shared_ptr<LLVMIRContext> ctx, Node type);

  /**
   * Gets the value of a Node.
   * @param node node to get value from.
   * @return the node's value.
   */
  std::string node_val(Node node);
}
