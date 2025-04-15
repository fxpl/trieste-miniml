#include "../../miniml-lang.hh"
#include "../internal.hh"
#include "../utils.hh"
#include "trieste/pass.h"
#include "trieste/rewrite.h"
#include "trieste/token.h"

// LLVM code builder
/**
 * This is a workaround to prevent warnings from LLVM libraries
 * being treated as errors by CMake.
 */
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wunused-parameter"
#pragma clang diagnostic ignored "-Wshadow"
#include "llvm/Support/raw_ostream.h"

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/NoFolder.h> // Ignore constant folding for educational purposes
#include <llvm/IR/Verifier.h>
#pragma clang diagnostic pop

namespace miniml {

  using namespace trieste;
  using namespace llvm;

  struct LLVMIRContext {
    // Holds core data structures: Type and constant value tables.
    llvm::LLVMContext llvm_context;
    // Generates LLVM IR instructions.
    // FIXME: NoFolder is used to prevent constant folding.
    llvm::IRBuilder<NoFolder> builder;
    // Contains generated instructions and local and global value tables.
    llvm::Module llvm_module;
    // Named variables, for keeping track of function arguments.
    std::map<std::string, Value*> namedVars;
    // Main function
    llvm::Function* mainFunction;

    // Maps register identifiers to their LLVM IR values.
    std::map<std::string, Value*> registers;

    // Maps temporary identifiers to actual function names.
    std::map<std::string, std::string> functions;

    LLVMIRContext()
    : builder(llvm_context), llvm_module("miniML", llvm_context) {}

    Value* result;

    ~LLVMIRContext() {}
  };

  // Helper function prototypes
  void genExternalFunctions(std::shared_ptr<LLVMIRContext> context);
  void genPrintInt(std::shared_ptr<LLVMIRContext> context);
  void genPrintBool(std::shared_ptr<LLVMIRContext> context);

  /**
   * @brief
   * This pass lowers to LLVM IR code.
   */
  PassDef generateLLVMIR() {
    // Context shareable for all matches in the pass.
    auto context = std::make_shared<LLVMIRContext>();

    /**
     * Bottom up pass that "recursively" generates LLVM IR.
     * By traversing the child nodes before the parent, storing the instruction
     * in the register map we can ensure that IR can be generated for the parent
     * node.
     *
     * Builder is used to create parts and store them in the node if it is a
     * component. If the node is an instruction, it fetches the LLVM IR Values
     * from the child nodes to emit the instruction.
     */
    PassDef
      pass =
        {
          "generateLLVMIR",
          LLVMIRGeneration::wf,
          dir::bottomup | dir::once,
          {
            /**
             * Binary Operations
             */
            T(Instr)[Instr]
                << (T(BinaryOp)
                    << (T(Add, Sub, Mul)[BinaryOp] << T(Ident)[Ident] *
                          T(Type) * T(Int, Ident)[Lhs] * T(Int, Ident)[Rhs])) >>
              [context](Match& _) -> Node {
              // FIXME: Using T(Add,Sub,MUL) & T(Int,Ident) as match patterns is
              // not scalable. Need to do something better

              // LeftHandSide
              Value* lhs = NULL;
              if (_(Lhs)->type() == Ident) {
                std::string ident = std::string(_(Lhs)->location().view());
                lhs = context->registers[ident];
              } else if (_(Lhs)->type() == Int) {
                lhs = context->builder.getInt32(
                  std::stoi(std::string(_(Lhs)->location().view())));
                ;
              }

              // RightHandSide
              Value* rhs = NULL;
              if (_(Rhs)->type() == Ident) {
                std::string ident = std::string(_(Rhs)->location().view());
                rhs = context->registers[ident];
              } else if (_(Rhs)->type() == Int) {
                rhs = context->builder.getInt32(
                  std::stoi(std::string(_(Rhs)->location().view())));
                ;
              }

              // Ensure register map has been correctly populated by previous
              // instructions.
              assert(lhs);
              assert(rhs);

              // Result.
              std::string resultId = std::string(_(Ident)->location().view());

              // Generate LLVM IR instruction.
              Token op = _(BinaryOp)->type();
              Value* result = NULL;
              if (op == Add) {
                result = context->builder.CreateAdd(lhs, rhs, resultId);
              } else if (op == Sub) {
                result = context->builder.CreateSub(lhs, rhs, resultId);
              } else if (op == Mul) {
                result = context->builder.CreateMul(lhs, rhs, resultId);
              }

              assert(result);

              // Map a register to the result.
              context->registers[resultId] = result;
              // Store the result as program result (This way the last
              // instruction sets the return value).
              context->result = result;

              // Do not alter the AST.
              return _(Instr);
            },

            /**
             * Memory Operations
             */
            // Alloca
            T(Instr)[Instr]
                << (T(MemoryOp)
                    << (T(Alloca)
                        << T(Ident)[Ident] * (T(Type) << T(TInt)[Type]))) >>
              [context](Match& _) -> Node {
              std::string resultId = std::string(_(Ident)->location().view());

              llvm::Type* type = NULL;
              if (_(Type)->type() == TInt) {
                type = context->builder.getInt32Ty();
              } else {
                // TODO: Error
              }

              AllocaInst* result =
                context->builder.CreateAlloca(type, nullptr, resultId);

              context->registers[resultId] = result;
              context->result = result;

              return _(Instr);
            },

            // Store
            T(Instr)[Instr]
                << (T(MemoryOp)
                    << (T(Store) << T(Ident)[IRValue] * T(Ident)[Dst])) >>
              [context](Match& _) -> Node {
              Value* value =
                context->registers[std::string(_(IRValue)->location().view())];
              Value* dst =
                context->registers[std::string(_(Dst)->location().view())];
              context->builder.CreateStore(value, dst);

              return _(Instr);
            },

            // Load
            T(Instr)[Instr]
                << (T(MemoryOp)
                    << (T(Load) << T(Ident)[Ident] *
                          (T(Type) << T(TInt)[Type]) * T(Ident)[Src])) >>
              [context](Match& _) -> Node {
              std::string resultId = std::string(_(Ident)->location().view());

              llvm::Type* type = NULL;
              if (_(Type)->type() == TInt) {
                type = context->builder.getInt32Ty();
              }

              assert(type);

              Value* src =
                context->registers[std::string(_(Src)->location().view())];

              assert(src);

              Value* result = context->builder.CreateLoad(type, src, resultId);

              context->registers[resultId] = result;
              context->result = result;

              return _(Instr);
            },

            /**
             * Misc. Operations
             */
            // Function Call
            T(Instr)
                << (T(MiscOp)
                    << (T(Call) << T(Ident)[Result] * T(Ident)[Fun] *
                          T(Ident)[Param])) >>
              [context](Match& _) -> Node {
              std::string paramId = std::string(_(Param)->location().view());
              Value* param = context->registers[paramId];
              assert(param);

              std::string tmpFuncId = std::string(_(Fun)->location().view());
              std::string funcName = context->functions[tmpFuncId];
              assert(!funcName.empty());

              Function* function = context->llvm_module.getFunction(funcName);
              assert(function);

              std::string resultId = std::string(_(Result)->location().view());
              Value* result =
                context->builder.CreateCall(function, {param}, resultId);

              context->registers[resultId] = result;
              context->result = result;

              return _(Instr);
            },

            // Compare
            T(Instr)[Instr]
                << (T(MiscOp)
                    << (T(Icmp) << T(Ident)[Ident] * T(Eq, Ult)[Op] *
                          T(Ti32)[Type] * T(Int, Ident)[Lhs] *
                          T(Int, Ident)[Rhs])) >>
              [context](Match& _) -> Node {
              std::cout << "comparison!!" << std::endl;

              if (_(Type) != Ti32) {
                return err(_(Type), "Not a valid type for comparison");
              }

              std::string lhsId = std::string(_(Lhs)->location().view());
              Value* lhs = context->registers[lhsId];
              assert(lhs);

              std::string rhsId = std::string(_(Rhs)->location().view());
              Value* rhs = context->registers[rhsId];
              assert(rhs);

              std::string resultId = std::string(_(Ident)->location().view());

              Value* result = NULL;
              if (_(Op) == Eq) {
                result = context->builder.CreateICmpEQ(lhs, rhs, resultId);
              } else if (_(Op) == Ult) {
                result = context->builder.CreateICmpULT(lhs, rhs, resultId);
              } else {
                return err(_(Op), "Unknown comparison operator");
              }
              assert(result);

              context->registers[resultId] = result;
              context->result = result;

              return _(Instr);
            },

            /**
             * Meta operations since LLVM IR does not allow assigning a register
             * to another
             */
            // Copy register value from Src to Dst.
            T(Meta) << (T(RegCpy) << T(Ident)[Dst] * T(Ident)[Src]) >>
              [context](Match& _) -> Node {
              std::string dst = std::string(_(Dst)->location().view());
              std::string src = std::string(_(Src)->location().view());
              Value* srcVal = context->registers[src];
              assert(srcVal);
              context->registers[dst] = srcVal;

              // Remove the meta node from the AST.
              return {};
            },

            // Map the temporary id `Ident` to function name `Fun`.
            T(Meta) << (T(FuncMap) << T(Ident)[Ident] * T(Ident)[Fun]) >>
              [context](Match& _) -> Node {
              std::string tmpIdent = std::string(_(Ident)->location().view());
              std::string functionName = std::string(_(Fun)->location().view());
              context->functions[tmpIdent] = functionName;

              // Remove the meta node from the AST.
              return {};
            },

            // Map the register `Ident` to Value* `IRValue`.
            T(Meta)
                << (T(RegMap) << T(Ident)[Ident] * T(Ti32, Ti1)[Type] *
                      T(IRValue)[IRValue]) >>
              [context](Match& _) -> Node {
              std::string regId = std::string(_(Ident)->location().view());
              std::string valueStr = std::string(_(IRValue)->location().view());

              Value* value = NULL;
              if (_(Type) == Ti32) {
                value = context->builder.getInt32(std::stoi(valueStr));
              } else if (_(Type) == Ti1) {
                value = context->builder.getInt1(std::stoi(valueStr));
              }
              assert(value);

              context->registers[regId] = value;
              context->result = value;

              // Remove the meta node from the AST.
              return {};
            },
          }};

    pass.pre([context](Node) {
      /**
       * External functions
       */
      auto ctx = context;

      genExternalFunctions(context);

      /**
       * Internal native functions
       */
      genPrintInt(context);
      genPrintBool(context);

      // Main(void) -> i32
      FunctionType* mainFuncType =
        FunctionType::get(Type::getInt32Ty(context->llvm_context), false);

      context->mainFunction = Function::Create(
        mainFuncType,
        Function::LinkageTypes::ExternalLinkage,
        "main",
        context->llvm_module);

      BasicBlock* entry = BasicBlock::Create(
        context->llvm_context, "entry", context->mainFunction);

      context->builder.SetInsertPoint(entry);

      return 0;
    });

    pass.post([context](Node) {
      Value* result = context->result;
      assert(result);

      llvm::Type* resultType = result->getType();

      Function* printFun = NULL;
      if (resultType == context->builder.getInt1Ty()) {
        printFun = context->llvm_module.getFunction("printBool");
      } else if (resultType == context->builder.getInt32Ty()) {
        printFun = context->llvm_module.getFunction("printInt");
      } else {
        // TODO: Error! Printtype not implemented!
      }
      assert(printFun);

      context->builder.CreateCall(printFun, {context->result});

      // Return 0 to indicate success.
      context->builder.CreateRet(context->builder.getInt32(0));

      Function* main = context->llvm_module.getFunction("main");
      verifyFunction(*main, &llvm::errs());
      verifyModule(context->llvm_module, &llvm::errs());

      // FIXME: Temporarily write generated LLVM IR to file so can be compiled
      // by make command.
      std::error_code errorCode;
      llvm::raw_fd_ostream outLLVMIR("out/test.ll", errorCode);
      context->llvm_module.print(outLLVMIR, nullptr);

      return 0;
    });

    return pass;
  }

  /**
   * Generates imports of external functions needed for LLVM IR generation.
   * @param context LLVM IR context
   */
  void genExternalFunctions(std::shared_ptr<LLVMIRContext> context) {
    // printf(char*) -> i32
    FunctionType* printfFunctionType = FunctionType::get(
      context->builder.getInt32Ty(),
      {context->builder.getInt8Ty()->getPointerTo()},
      true);
    context->llvm_module.getOrInsertFunction("printf", printfFunctionType);
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
    FunctionType* printIntType = FunctionType::get(
      context->builder.getInt32Ty(), {context->builder.getInt32Ty()}, false);

    Function* printInt = Function::Create(
      printIntType,
      Function::LinkageTypes::ExternalLinkage,
      "printInt",
      context->llvm_module);

    printInt->setCallingConv(CallingConv::C);

    BasicBlock* printIntBody =
      BasicBlock::Create(context->llvm_context, "printIntBody", printInt);
    context->builder.SetInsertPoint(printIntBody);

    Argument* arg = printInt->arg_begin();
    arg->setName("intToPrint");

    Function* printfFunc = context->llvm_module.getFunction("printf");
    context->builder.CreateCall(printfFunc, {formatStrInt, arg});

    context->builder.CreateRet(arg);

    verifyFunction(*printInt, &llvm::errs());
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
    FunctionType* printBoolType = FunctionType::get(
      context->builder.getInt1Ty(), {context->builder.getInt1Ty()}, false);

    Function* printBool = Function::Create(
      printBoolType,
      Function::LinkageTypes::ExternalLinkage,
      "printBool",
      context->llvm_module);

    printBool->setCallingConv(CallingConv::C);

    BasicBlock* printBoolBody =
      BasicBlock::Create(context->llvm_context, "printBoolBody", printBool);
    context->builder.SetInsertPoint(printBoolBody);

    Argument* arg = printBool->arg_begin();
    arg->setName("boolToPrint");

    Function* printfFunc = context->llvm_module.getFunction("printf");
    context->builder.CreateCall(
      printfFunc,
      {context->builder.CreateSelect(arg, strBoolTrue, strBoolFalse)});

    context->builder.CreateRet(arg);

    verifyFunction(*printBool, &llvm::errs());
  }

}
