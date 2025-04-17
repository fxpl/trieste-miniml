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

    // Maps temporary identifiers basic blocks.
    std::map<std::string, llvm::BasicBlock*> basicBlocks;

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
          dir::topdown | dir::once,
          {
            /**
             * Binary Operations
             */
            T(Instr)[Instr]
                << (T(BinaryOp)
                    << (T(Add, Sub, Mul)[Op] << T(Ident)[Ident] * T(Type) *
                          T(Int, Ident)[Lhs] * T(Int, Ident)[Rhs])) >>
              [context](Match& _) -> Node {
              // FIXME: Using T(Add,Sub,MUL) & T(Int,Ident) as match patterns is
              // not scalable. Need to do something better

              std::string lhsId = node_val(_(Lhs));
              Value* lhs = context->registers[lhsId];
              assert(lhs);

              std::string rhsId = node_val(_(Rhs));
              Value* rhs = context->registers[rhsId];
              assert(rhs);

              std::string resultId = node_val(_(Ident));
              Value* result = NULL;
              if (_(Op) == Add) {
                result = context->builder.CreateAdd(lhs, rhs, resultId);
              } else if (_(Op) == Sub) {
                result = context->builder.CreateSub(lhs, rhs, resultId);
              } else if (_(Op) == Mul) {
                result = context->builder.CreateMul(lhs, rhs, resultId);
              }
              assert(result);

              context->registers[resultId] = result;
              context->result = result;

              return _(Instr);
            },

            /**
             * Memory Operations
             */
            // Alloca
            T(Instr)[Instr]
                << (T(MemoryOp)
                    << (T(Alloca) << T(Ident)[Ident] * (T(Ti32, Ti1)[Type]))) >>
              [context](Match& _) -> Node {
              std::string resultId = node_val(_(Ident));

              llvm::Type* llvmType = NULL;
              if (_(Type) == Ti32) {
                llvmType = context->builder.getInt32Ty();
              } else if (_(Type) == Ti1) {
                llvmType = context->builder.getInt1Ty();
              }
              assert(llvmType);

              AllocaInst* result =
                context->builder.CreateAlloca(llvmType, nullptr, resultId);

              context->registers[resultId] = result;
              context->result = result;

              return _(Instr);
            },

            // Store
            T(Instr)[Instr]
                << (T(MemoryOp)
                    << (T(Store) << T(Ident)[IRValue] * T(Ident)[Dst])) >>
              [context](Match& _) -> Node {
              std::string valueId = node_val(_(IRValue));
              Value* value = context->registers[valueId];
              assert(value);

              std::string destId = node_val(_(Dst));
              Value* dest = context->registers[destId];
              assert(dest);

              context->builder.CreateStore(value, dest);

              return _(Instr);
            },

            // Load
            T(Instr)[Instr]
                << (T(MemoryOp)
                    << (T(Load) << T(Ident)[Ident] *
                          T(Ti32, Ti1)[Type] * T(Ident)[Src])) >>
              [context](Match& _) -> Node {
              // TODO: Handle other types
              std::string resultId = node_val(_(Ident));

              llvm::Type* type = NULL;
              if (_(Type) == Ti32) {
                type = context->builder.getInt32Ty();
              } else if (_(Type) == Ti1) {
                type = context->builder.getInt1Ty();
              }
              assert(type);

              Value* src = context->registers[node_val(_(Src))];
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
              std::string paramId = node_val(_(Param));
              Value* param = context->registers[paramId];
              assert(param);

              std::string tmpFuncId = node_val(_(Fun));
              std::string funcName = context->functions[tmpFuncId];
              assert(!funcName.empty());

              Function* function = context->llvm_module.getFunction(funcName);
              assert(function);

              std::string resultId = node_val(_(Result));
              Value* result =
                context->builder.CreateCall(function, {param}, resultId);

              context->registers[resultId] = result;
              context->result = result;

              return _(Instr);
            },

            // Compare
            T(Instr)[Instr]
                << (T(MiscOp)
                    << (T(Icmp) << T(Ident)[Ident] * T(EQ, ULT)[Op] *
                          T(Ti32)[Type] * T(Int, Ident)[Lhs] *
                          T(Int, Ident)[Rhs])) >>
              [context](Match& _) -> Node {
              if (_(Type) != Ti32) {
                return err(_(Type), "Not a valid type for comparison");
              }

              std::string lhsId = node_val(_(Lhs));
              Value* lhs = context->registers[lhsId];
              assert(lhs);

              std::string rhsId = node_val(_(Rhs));
              Value* rhs = context->registers[rhsId];
              assert(rhs);

              std::string resultId = node_val(_(Ident));

              Value* result = NULL;
              if (_(Op) == EQ) {
                result = context->builder.CreateICmpEQ(lhs, rhs, resultId);
              } else if (_(Op) == ULT) {
                result = context->builder.CreateICmpULT(lhs, rhs, resultId);
              } else {
                return err(_(Op), "Unknown comparison operator");
              }
              assert(result);

              context->registers[resultId] = result;
              context->result = result;

              return _(Instr);
            },

            // Phi
            T(Instr)[Instr]
                << (T(MiscOp)
                    << (T(Phi) << T(Ident)[Ident] * T(Ti32, Ti1)[Type] *
                          (T(Predecessor)
                           << T(Prev)[True] * T(Prev)[False]))) >>
              [context](Match& _) -> Node {
              llvm::Type* type = NULL;
              if (_(Type) == Ti32) {
                type = context->builder.getInt32Ty();
              } else if (_(Type) == Ti1) {
                type = context->builder.getInt1Ty();
              }
              assert(type);

              std::string ifTrueId = node_val(_(True) / IRValue);
              Value* trueVal = context->registers[ifTrueId];
              assert(trueVal);

              std::string trueLabel = node_val(_(True) / Label);
              BasicBlock* trueBlock = context->basicBlocks[trueLabel];
              assert(trueBlock);

              std::string falseId = node_val(_(False) / IRValue);
              Value* falseVal = context->registers[falseId];
              assert(falseVal);

              std::string falseLabel = node_val(_(False) / Label);
              BasicBlock* falseBlock = context->basicBlocks[falseLabel];
              assert(falseBlock);

              std::string resultId = node_val(_(Ident));
              PHINode* phi = context->builder.CreatePHI(type, 2, resultId);
              phi->addIncoming(trueVal, trueBlock);
              phi->addIncoming(falseVal, falseBlock);

              context->registers[resultId] = phi;
              context->result = phi;

              return _(Instr);
            },

            // Label
            T(Label) << T(Ident)[Ident] >> [context](Match& _) -> Node {
              std::string blockId = node_val(_(Ident));
              llvm::BasicBlock* block = context->basicBlocks[blockId];
              assert(block);

              context->builder.SetInsertPoint(block);

              return _(Label);
            },

            /**
             * Terminating Operations.
             */
            // Branch
            T(Instr)[Instr]
                << (T(TerminatorOp)
                    << (T(Branch) << T(Ident)[Cond] * T(Ident)[True] *
                          T(Ident)[False])) >>
              [context](Match& _) -> Node {
              std::string condId = node_val(_(Cond));
              Value* cond = context->registers[condId];
              assert(cond);

              std::string trueId = node_val(_(True));
              llvm::BasicBlock* trueBlock = context->basicBlocks[trueId];
              assert(trueBlock);

              std::string falseId = node_val(_(False));
              llvm::BasicBlock* falseBlock = context->basicBlocks[falseId];
              assert(falseBlock);

              context->builder.CreateCondBr(cond, trueBlock, falseBlock);

              return _(Instr);
            },

            // Jump
            T(Instr)[Instr]
                << (T(TerminatorOp) << (T(Jump) << T(Ident)[Label])) >>
              [context](Match& _) -> Node {
              std::string blockId = node_val(_(Label));
              llvm::BasicBlock* block = context->basicBlocks[blockId];
              assert(block);

              context->builder.CreateBr(block);

              return _(Instr);
            },

            /**
             * Meta operations since LLVM IR does not allow assigning a register
             * to another
             */
            // Copy register value from Src to Dst.
            T(Meta) << (T(RegCpy) << T(Ident)[Dst] * T(Ident)[Src]) >>
              [context](Match& _) -> Node {
              std::string dst = node_val(_(Dst));
              std::string src = node_val(_(Src));
              Value* srcVal = context->registers[src];
              assert(srcVal);
              context->registers[dst] = srcVal;

              // Remove the meta node from the AST.
              return {};
            },

            // Map the temporary id `Ident` to function name `Fun`.
            T(Meta) << (T(FuncMap) << T(Ident)[Ident] * T(Ident)[Fun]) >>
              [context](Match& _) -> Node {
              std::string tmpIdent = node_val(_(Ident));
              std::string functionName = node_val(_(Fun));
              context->functions[tmpIdent] = functionName;

              // Remove the meta node from the AST.
              return {};
            },

            // Map the temporary `Ident` to Value* `IRValue`.
            T(Meta)
                << (T(RegMap) << T(Ident)[Ident] * T(Ti32, Ti1)[Type] *
                      T(IRValue)[IRValue]) >>
              [context](Match& _) -> Node {
              std::string regId = node_val(_(Ident));
              std::string valueStr = node_val(_(IRValue));

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

            // Create Basic Block
            T(Meta) << (T(BlockMap) << T(Ident)[Ident]) >>
              [context](Match& _) -> Node {
              std::string blockId = node_val(_(Ident));

              // TODO: How to keep track of parent when this is in the body of a
              // function!?
              BasicBlock* block = BasicBlock::Create(
                context->llvm_context, blockId, context->mainFunction);

              context->basicBlocks[blockId] = block;

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
