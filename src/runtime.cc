#include "runtime.hh"

#include <llvm/IR/Verifier.h>

namespace llvmir {
  using namespace llvm;

  // Private function prototypes
  void genPrintInt(std::shared_ptr<LLVMIRContext> context);
  void genPrintBool(std::shared_ptr<LLVMIRContext> context);

  void genExternalFunctions(std::shared_ptr<LLVMIRContext> ctx) {
    // printf(char*) -> i32
    FunctionType* printfFunctionType = FunctionType::get(
      ctx->builder.getInt32Ty(),
      {ctx->builder.getInt8Ty()->getPointerTo()},
      true);
    ctx->llvm_module.getOrInsertFunction("printf", printfFunctionType);

    // malloc(i64) -> ptr
    FunctionType* mallocFunctionType = FunctionType::get(
      ctx->builder.getPtrTy(), {ctx->builder.getInt64Ty()}, false);
    ctx->llvm_module.getOrInsertFunction("malloc", mallocFunctionType);
  }

  void genRuntimeFunctions(std::shared_ptr<LLVMIRContext> context) {
    // Allows MiniML's 'print' expression to print to stdout.
    genPrintInt(context);
    genPrintBool(context);

    // Closure type in order to compile higher-order functions
    StructType* ClosureTy =
      StructType::create(context->llvm_context, "ClosureTy");
    ClosureTy->setBody(
      {context->builder.getPtrTy(), context->builder.getPtrTy()}, false);

    context->types["ClosureTy"] = ClosureTy;
  }

  /**
   * Generates a function that prints an integer value.
   * @param context LLVM IR context
   */
  void genPrintInt(std::shared_ptr<LLVMIRContext> context) {
    // Formatstring needed to make calls to printf
    auto formatStrInt = context->builder.CreateGlobalStringPtr(
      "%d\n", "formatStrInt", 0, &context->llvm_module);

    // printInt(i32) -> i32
    FunctionType* theFunctionType = FunctionType::get(
      context->builder.getInt32Ty(), {context->builder.getInt32Ty()}, false);

    std::string functionName = "native$printInt";
    Function* theFunction = Function::Create(
      theFunctionType,
      Function::LinkageTypes::ExternalLinkage,
      functionName,
      context->llvm_module);
    theFunction->setCallingConv(CallingConv::C);
    context->registers[functionName] = theFunction;

    BasicBlock* functionEntryBlock =
      BasicBlock::Create(context->llvm_context, "entry", theFunction);
    context->builder.SetInsertPoint(functionEntryBlock);

    Argument* arg = theFunction->arg_begin();
    arg->setName("intToPrint");

    Function* printfFunc = context->llvm_module.getFunction("printf");
    context->builder.CreateCall(printfFunc, {formatStrInt, arg});

    context->builder.CreateRet(arg);

    verifyFunction(*theFunction, &llvm::errs());
  }

  /**
   * Generates a function that prints a boolean value.
   * @param context LLVM IR context
   */
  void genPrintBool(std::shared_ptr<LLVMIRContext> context) {
    // String needed to make calls to printf
    auto strBoolTrue = context->builder.CreateGlobalStringPtr(
      "true\n", "strBoolTrue", 0, &context->llvm_module);
    auto strBoolFalse = context->builder.CreateGlobalStringPtr(
      "false\n", "strBoolFalse", 0, &context->llvm_module);

    // printInt(i1) -> i1
    FunctionType* theFunctionType = FunctionType::get(
      context->builder.getInt1Ty(), {context->builder.getInt1Ty()}, false);

    std::string functionName = "native$printBool";
    Function* theFunction = Function::Create(
      theFunctionType,
      Function::LinkageTypes::ExternalLinkage,
      functionName,
      context->llvm_module);
    theFunction->setCallingConv(CallingConv::C);
    context->registers[functionName] = theFunction;

    BasicBlock* functionEntryBlock =
      BasicBlock::Create(context->llvm_context, "entry", theFunction);
    context->builder.SetInsertPoint(functionEntryBlock);

    Argument* arg = theFunction->arg_begin();
    arg->setName("boolToPrint");

    Function* printfFunc = context->llvm_module.getFunction("printf");
    context->builder.CreateCall(
      printfFunc,
      {context->builder.CreateSelect(arg, strBoolTrue, strBoolFalse)});

    context->builder.CreateRet(arg);

    verifyFunction(*theFunction, &llvm::errs());
  }

}
