#include "../../miniml-lang.hh"
#include "../internal.hh"
#include "../llvm_utils.hh"
#include "../utils.hh"
#include "trieste/pass.h"
#include "trieste/rewrite.h"
#include "trieste/token.h"

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wunused-parameter"
#pragma clang diagnostic ignored "-Wshadow"
#include "llvm/Support/raw_ostream.h"

#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Verifier.h>
#pragma clang diagnostic pop

namespace miniml {

  using namespace trieste;
  using namespace llvm;

  // Helper function prototypes
  void genExternalFunctions(std::shared_ptr<LLVMIRContext> context);
  void genPrintInt(std::shared_ptr<LLVMIRContext> context);
  void genPrintBool(std::shared_ptr<LLVMIRContext> context);

  /**
   * @brief
   * This pass lowers to LLVM IR code.
   */
  PassDef generateLLVMIR() {
    auto ctx = std::make_shared<LLVMIRContext>();

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
                    << (T(Add, Sub, Mul)[Op] << T(Ident)[Ident] * T(Ti32) *
                          T(Ident)[Lhs] * T(Ident)[Rhs])) >>
              [ctx](Match& _) -> Node {
              std::string lhsId = node_val(_(Lhs));
              Value* lhs = ctx->registers[lhsId];
              assert(lhs);

              std::string rhsId = node_val(_(Rhs));
              Value* rhs = ctx->registers[rhsId];
              assert(rhs);

              std::string resultId = node_val(_(Ident));
              Value* result = NULL;
              if (_(Op) == Add) {
                result = ctx->builder.CreateAdd(lhs, rhs, resultId);
              } else if (_(Op) == Sub) {
                result = ctx->builder.CreateSub(lhs, rhs, resultId);
              } else if (_(Op) == Mul) {
                result = ctx->builder.CreateMul(lhs, rhs, resultId);
              }
              assert(result);

              ctx->registers[resultId] = result;

              return _(Instr);
            },

            /**
             * Memory Operations
             */
            // Alloca
            T(Instr)[Instr]
                << (T(MemoryOp)
                    << (T(Alloca) << T(Ident)[Ident] *
                          (T(Ti64, Ti32, Ti1, TPtr)[Type]))) >>
              [ctx](Match& _) -> Node {
              std::string resultId = node_val(_(Ident));

              llvm::Type* llvmType = createLLVMType(ctx, _(Type));
              assert(llvmType);

              AllocaInst* result =
                ctx->builder.CreateAlloca(llvmType, nullptr, resultId);

              ctx->registers[resultId] = result;

              return _(Instr);
            },

            // Store
            T(Instr)[Instr]
                << (T(MemoryOp)
                    << (T(Store) << T(Ident)[IRValue] * T(Ident)[Dst])) >>
              [ctx](Match& _) -> Node {
              std::string valueId = node_val(_(IRValue));
              Value* value = ctx->registers[valueId];
              assert(value);

              std::string destId = node_val(_(Dst));
              Value* dest = ctx->registers[destId];
              assert(dest);

              ctx->builder.CreateStore(value, dest);

              return _(Instr);
            },

            // Load
            T(Instr)[Instr]
                << (T(MemoryOp)
                    << (T(Load) << T(Ident)[Ident] * T(Ti32, Ti1, TPtr)[Type] *
                          T(Ident)[Src])) >>
              [ctx](Match& _) -> Node {
              std::string resultId = node_val(_(Ident));

              llvm::Type* type = createLLVMType(ctx, _(Type));
              assert(type);

              Value* src = ctx->registers[node_val(_(Src))];
              assert(src);

              Value* result = ctx->builder.CreateLoad(type, src, resultId);

              ctx->registers[resultId] = result;

              return _(Instr);
            },

            // GetElementPointer
            T(Instr)[Instr]
                << (T(MemoryOp)
                    << (T(GetElementPtr)
                        << (T(Ident)[Result] * T(Ident)[IRType] *
                            T(Ident)[IRValue] * T(OffsetList)[OffsetList]))) >>
              [ctx](Match& _) -> Node {
              std::string valueId = node_val(_(IRValue));
              Value* value = ctx->registers[valueId];
              assert(value);

              std::string typeId = node_val(_(IRType));
              llvm::Type* type = ctx->types[typeId];
              assert(type);

              std::vector<Value*> offsets;
              for (size_t i = 0; i < _(OffsetList)->size(); i++) {
                Node offsetNode = _(OffsetList)->at(i);

                std::string valStr = node_val(offsetNode / IRValue);
                Node offsetType = offsetNode / IRType;
                Value* offset = nullptr;
                if (offsetType == Ti32) {
                  int offsetValue = stoi(valStr);
                  offset = ctx->builder.getInt32(offsetValue);
                }
                assert(offset && "GEP - unexpected offset type");

                offsets.push_back(offset);
              }

              std::string resultId = node_val(_(Result));
              Value* result =
                ctx->builder.CreateGEP(type, value, offsets, resultId);
              ctx->registers[resultId] = result;

              return _(Instr);
            },

            /**
             * Misc. Operations
             */
            // Function Call
            T(Instr)[Instr]
                << (T(MiscOp)
                    << (T(Call) << T(Ident)[Result] * T(Ident)[Fun] *
                          T(ArgList)[ArgList])) >>
              [ctx](Match& _) -> Node {
              std::string funId = node_val(_(Fun));
              Function* function = ctx->llvm_module.getFunction(funId);

              if (function == nullptr) {
                function = (Function*)ctx->registers[funId];
              }
              assert(function);

              std::vector<llvm::Value*> arguments;
              for (size_t i = 0; i < _(ArgList)->size(); i++) {
                Node arg = _(ArgList)->at(i);
                std::string argId = node_val(arg);
                Value* argVal = ctx->registers[argId];
                assert(argVal);

                arguments.push_back(argVal);
              }

              std::string resultId = node_val(_(Result));
              Value* result =
                ctx->builder.CreateCall(function, arguments, resultId);

              ctx->registers[resultId] = result;

              return _(Instr);
            },

            // Function Call (Opaque)
            T(Instr)[Instr]
                << (T(MiscOp)
                    << (T(CallOpaque) << T(Ident)[Result] * T(Ident)[IRType] *
                          T(Ident)[Fun] * T(ArgList)[ArgList])) >>
              [ctx](Match& _) -> Node {
              std::string funTyId = node_val(_(IRType));
              llvm::FunctionType* funTy =
                (llvm::FunctionType*)ctx->types[funTyId];
              assert(funTy);

              std::string funId = node_val(_(Fun));
              llvm::Value* functionPtr = ctx->registers[funId];
              assert(functionPtr);

              std::vector<llvm::Value*> arguments;
              for (size_t i = 0; i < _(ArgList)->size(); i++) {
                Node arg = _(ArgList)->at(i);
                std::string argId = node_val(arg);
                Value* argVal = ctx->registers[argId];
                assert(argVal);

                arguments.push_back(argVal);
              }

              std::string resultId = node_val(_(Result));
              Value* result = ctx->builder.CreateCall(
                funTy, functionPtr, arguments, resultId);

              ctx->registers[resultId] = result;

              return _(Instr);
            },

            // Compare
            T(Instr)[Instr]
                << (T(MiscOp)
                    << (T(Icmp) << T(Ident)[Ident] * T(EQ, ULT, SLT)[Op] *
                          T(Ti32, Ti1)[Type] * T(Ident)[Lhs] *
                          T(Ident)[Rhs])) >>
              [ctx](Match& _) -> Node {
              std::string lhsId = node_val(_(Lhs));
              Value* lhs = ctx->registers[lhsId];
              assert(lhs);

              std::string rhsId = node_val(_(Rhs));
              Value* rhs = ctx->registers[rhsId];
              assert(rhs);

              std::string resultId = node_val(_(Ident));

              Value* result = NULL;
              Node op = _(Op);
              if (op == EQ) {
                result = ctx->builder.CreateICmpEQ(lhs, rhs, resultId);
              } else if (op == ULT) {
                result = ctx->builder.CreateICmpULT(lhs, rhs, resultId);
              } else if (op == SLT) {
                result = ctx->builder.CreateICmpSLT(lhs, rhs, resultId);
              } else {
                return err(op, "Unknown comparison operator");
              }
              assert(result);

              ctx->registers[resultId] = result;

              return _(Instr);
            },

            // Phi
            T(Instr)[Instr]
                << (T(MiscOp)
                    << (T(Phi) << T(Ident)[Ident] * T(Ti32, Ti1)[Type] *
                          (T(PredecessorList)
                           << T(Predecessor)[True] * T(Predecessor)[False]))) >>
              [ctx](Match& _) -> Node {
              llvm::Type* type = NULL;
              if (_(Type) == Ti32) {
                type = ctx->builder.getInt32Ty();
              } else if (_(Type) == Ti1) {
                type = ctx->builder.getInt1Ty();
              }
              assert(type);

              std::string ifTrueId = node_val(_(True) / IRValue);
              Value* trueVal = ctx->registers[ifTrueId];
              assert(trueVal);

              std::string trueLabel = node_val(_(True) / Label);
              BasicBlock* trueBlock = ctx->basicBlocks[trueLabel];
              assert(trueBlock);

              std::string falseId = node_val(_(False) / IRValue);
              Value* falseVal = ctx->registers[falseId];
              assert(falseVal);

              std::string falseLabel = node_val(_(False) / Label);
              BasicBlock* falseBlock = ctx->basicBlocks[falseLabel];
              assert(falseBlock);

              std::string resultId = node_val(_(Ident));
              PHINode* phi = ctx->builder.CreatePHI(type, 2, resultId);
              phi->addIncoming(trueVal, trueBlock);
              phi->addIncoming(falseVal, falseBlock);

              ctx->registers[resultId] = phi;

              return _(Instr);
            },

            // Label
            In(Block) * T(Label)[Label] >> [ctx](Match& _) -> Node {
              std::string funId = node_val(_(Label)->parent(IRFun));
              std::string blockId = node_val(_(Label));

              llvm::BasicBlock* block = ctx->basicBlocks[blockId];
              assert(block);

              ctx->builder.SetInsertPoint(block);

              return _(Label);
            },

            /**
             * Terminating Operations.
             */
            // Branch
            T(Instr)[Instr]
                << (T(TerminatorOp)
                    << (T(Branch)
                        << (T(Ident)[Cond] * T(Label)[True] *
                            T(Label)[False]))) >>
              [ctx](Match& _) -> Node {
              std::string condId = node_val(_(Cond));
              Value* cond = ctx->registers[condId];
              assert(cond);

              std::string trueId = node_val(_(True));
              llvm::BasicBlock* trueBlock = ctx->basicBlocks[trueId];
              assert(trueBlock);

              std::string falseId = node_val(_(False));
              llvm::BasicBlock* falseBlock = ctx->basicBlocks[falseId];
              assert(falseBlock);

              ctx->builder.CreateCondBr(cond, trueBlock, falseBlock);

              return _(Instr);
            },

            // Jump
            T(Instr)[Instr]
                << (T(TerminatorOp) << (T(Jump) << (T(Label)[Label]))) >>
              [ctx](Match& _) -> Node {
              std::string blockId = node_val(_(Label));
              llvm::BasicBlock* block = ctx->basicBlocks[blockId];
              assert(block);

              ctx->builder.CreateBr(block);

              return _(Instr);
            },

            // Return
            T(Instr)[Instr]
                << (T(TerminatorOp) << (T(Ret) << T(Ident)[Ident])) >>
              [ctx](Match& _) -> Node {
              std::string resultId = node_val(_(Ident));
              Value* result = ctx->registers[resultId];
              assert(result);

              ctx->builder.CreateRet(result);

              return _(Instr);
            },

            /**
             * Conversion Operations
             */
            // BitCast (Custom Type)
            T(Instr)[Instr]
                << (T(ConversionOp)
                    << (T(BitCast) << T(Ident)[Result] * T(Ident)[IRValue] *
                          T(Ident)[IRType])) >>
              [ctx](Match& _) -> Node {
              std::string targetTypeId = node_val(_(IRType));
              llvm::Type* targetType = ctx->types[targetTypeId];
              assert(targetType);

              std::string valueToConvertId = node_val(_(IRValue));
              Value* valueToConvert = ctx->registers[valueToConvertId];
              assert(valueToConvert);

              std::string resultId = node_val(_(Result));
              Value* result = ctx->builder.CreateBitCast(
                valueToConvert, targetType, resultId);
              ctx->registers[resultId] = result;

              return _(Instr);
            },

            // BitCast (Fixed Type)
            T(Instr)[Instr]
                << (T(ConversionOp)
                    << (T(BitCast) << T(Ident)[Result] * T(Ident)[IRValue] *
                          T(Ti1, Ti32, Ti64, TPtr)[IRType])) >>
              [ctx](Match& _) -> Node {
              llvm::Type* targetType = createLLVMType(ctx, _(IRType));
              assert(targetType);

              std::string valueToConvertId = node_val(_(IRValue));
              Value* valueToConvert = ctx->registers[valueToConvertId];
              assert(valueToConvert);

              std::string resultId = node_val(_(Result));
              Value* result = ctx->builder.CreateBitCast(
                valueToConvert, targetType, resultId);
              ctx->registers[resultId] = result;

              return _(Instr);
            },

            /**
             * Function Declaration
             */
            T(IRFun)[Fun] << T(TypeArrow)[TypeArrow] * T(ParamList)[ParamList] *
                  T(Body) >>
              [ctx](Match& _) -> Node {
              Node returnType = _(TypeArrow) / Ty2;
              llvm::Type* returnLLVMType = createLLVMType(ctx, returnType);
              assert(returnLLVMType);

              std::vector<llvm::Type*> paramTypes;
              for (size_t i = 0; i < _(ParamList)->size(); i++) {
                Node param = _(ParamList)->at(i);
                Node type = param / Type;
                llvm::Type* llvmType = createLLVMType(ctx, type);
                assert(llvmType);

                paramTypes.push_back(llvmType);
              }

              FunctionType* theFunctionType = nullptr;
              if (_(ParamList)->size() == 0) {
                // Handle nullary functions
                theFunctionType = FunctionType::get(returnLLVMType, false);
              } else {
                theFunctionType =
                  FunctionType::get(returnLLVMType, paramTypes, false);
              }
              assert(theFunctionType);

              std::string theFunctionName = node_val(_(Fun));
              Function* theFunction = Function::Create(
                theFunctionType,
                Function::LinkageTypes::ExternalLinkage,
                theFunctionName,
                ctx->llvm_module);
              theFunction->setCallingConv(CallingConv::C);
              ctx->registers[theFunctionName] = theFunction;

              return _(Fun);
            },

            /**
             * Actions performed by LLVM IR builder.
             */
            // Get Type
            T(Action)[Action]
                << (T(GetType) << T(Ident)[Result] * T(Ident)[IRType]) >>
              [ctx](Match& _) -> Node {
              std::string valueId = node_val(_(IRType));
              llvm::Value* value = ctx->registers[valueId];
              assert(value);

              llvm::Type* destType = value->getType();

              std::string resultId = node_val(_(Result));
              ctx->types[resultId] = destType;

              return _(Action);
            },

            // Get Function
            T(Action)[Action]
                << (T(GetFunction) << T(Ident)[Result] * T(Ident)[Fun]) >>
              [ctx](Match& _) -> Node {
              std::string tmpIdent = node_val(_(Result));
              std::string functionName = node_val(_(Fun));
              Function* theFunction =
                ctx->llvm_module.getFunction(functionName);
              assert(theFunction);

              ctx->registers[tmpIdent] = theFunction;

              return _(Action);
            },

            // Create Struct Type
            T(Action)[Action]
                << (T(CreateStructType)
                    << T(Ident)[Result] * T(IRTypeList)[IRTypeList]) >>
              [ctx](Match& _) -> Node {
              std::vector<llvm::Type*> fieldTypes;
              for (auto irType : *_(IRTypeList)) {
                llvm::Type* llvmType = createLLVMType(ctx, irType);
                assert(llvmType);

                fieldTypes.push_back(llvmType);
              }

              std::string resultId = node_val(_(Result));
              llvm::StructType* theStructType =
                llvm::StructType::create(ctx->llvm_context, resultId);
              theStructType->setBody(fieldTypes, false);
              assert(theStructType);

              ctx->types[resultId] = theStructType;

              return _(Action);
            },

            // Create Function Type
            T(Action)[Action]
                << (T(CreateFunType) << T(Ident)[Result] *
                      T(Ti1, Ti32, Ti64, TPtr)[IRType] *
                      T(IRTypeList)[ParamList]) >>
              [ctx](Match& _) -> Node {
              Node returnType = _(IRType);
              llvm::Type* returnLLVMType = createLLVMType(ctx, returnType);
              assert(returnLLVMType);

              std::vector<llvm::Type*> paramTypes;
              for (size_t i = 0; i < _(ParamList)->size(); i++) {
                Node paramTy = _(ParamList)->at(i);
                llvm::Type* llvmType = createLLVMType(ctx, paramTy);
                assert(llvmType);

                paramTypes.push_back(llvmType);
              }

              FunctionType* theFunctionType = nullptr;
              if (_(ParamList)->size() == 0) {
                // Handle nullary functions
                theFunctionType = FunctionType::get(returnLLVMType, false);
              } else {
                theFunctionType =
                  FunctionType::get(returnLLVMType, paramTypes, false);
              }
              assert(theFunctionType);

              std::string resultId = node_val(_(Result));
              ctx->types[resultId] = theFunctionType;

              return _(Action);
            },

            // Create Constant
            T(Action)[Action]
                << (T(CreateConst)
                    << (T(Ident)[Ident] * T(Ti64, Ti32, Ti1)[Type] *
                        T(IRValue)[IRValue])) >>
              [ctx](Match& _) -> Node {
              std::string regId = node_val(_(Ident));
              std::string valueStr = node_val(_(IRValue));

              Value* value = NULL;
              if (_(Type) == Ti1) {
                value = ctx->builder.getInt1(std::stoi(valueStr));
              } else if (_(Type) == Ti32) {
                value = ctx->builder.getInt32(std::stoi(valueStr));
              } else if (_(Type) == Ti64) {
                value = ctx->builder.getInt64(std::stoi(valueStr));
              }
              assert(value);

              ctx->registers[regId] = value;

              return _(Action);
            },

            // Get Type Size
            T(Action) << T(GetSizeOfType)[GetSizeOfType] >>
              [ctx](Match& _) -> Node {
              Node getTypeSize = _(GetSizeOfType);
              Node result = getTypeSize / Result;
              Node desiredType = getTypeSize / Type;
              Node typeToFindSizeOf = getTypeSize / IRType;

              std::string resultId = node_val(result);
              std::string typeId = node_val(typeToFindSizeOf);
              llvm::Type* type = ctx->types[typeId];
              assert(type);

              auto dl = llvm::DataLayout(&ctx->llvm_module);
              auto typeSize = dl.getTypeStoreSize(type);

              llvm::Value* value = nullptr;
              if (desiredType == Ti32) {
                value = ctx->builder.getInt32(typeSize);
              } else if (desiredType == Ti64) {
                value = ctx->builder.getInt64(typeSize);
              }
              assert(value);

              ctx->registers[resultId] = value;

              return NoChange;
            },

            // Create Basic Block
            In(Body) * T(Block)[Block] >> [ctx](Match& _) -> Node {
              std::string funName = node_val(_(Block)->parent(IRFun));
              Function* function = ctx->llvm_module.getFunction(funName);
              assert(function);

              std::string blockId = node_val(_(Block));
              BasicBlock* block =
                BasicBlock::Create(ctx->llvm_context, blockId, function);

              ctx->basicBlocks[blockId] = block;

              return _(Block);
            },

          }};

    pass.pre([ctx](Node) {
      // TODO: Refactor these to separate file to decouple code gen from source
      // language.
      /**
       * External functions
       */
      genExternalFunctions(ctx);

      /**
       * Internal native functions
       */
      genPrintInt(ctx);
      genPrintBool(ctx);

      // Declare a closure type
      StructType* ClosureTy =
        StructType::create(ctx->llvm_context, "ClosureTy");
      ClosureTy->setBody(
        {ctx->builder.getPtrTy(), ctx->builder.getPtrTy()}, false);

      ctx->types["ClosureTy"] = ClosureTy;

      return 0;
    });

    pass.pre(IRFun, [ctx](Node functionToken) {
      ctx->registers.clear();
      std::string funId = node_val(functionToken);
      llvm::Function* function = ctx->llvm_module.getFunction(funId);

      Argument* arg = function->arg_begin();
      Node paramList = functionToken / ParamList;
      for (size_t i = 0; i < paramList->size(); i++) {
        Node param = paramList->at(i);
        std::string paramName = node_val(param / Ident);
        arg->setName(paramName);
        ctx->registers[paramName] = arg;
        arg++;
      }

      return 0;
    });

    pass.post([ctx](Node) {
      // FIXME: Should verify all functions.
      Function* main = ctx->llvm_module.getFunction("main");
      verifyFunction(*main, &llvm::errs());
      verifyModule(ctx->llvm_module, &llvm::errs());

      // TODO: Figure out how to output. into file? into clang via API?
      // FIXME: Temporarily write generated LLVM IR to file so can be compiled
      // by make command.
      std::error_code errorCode;
      llvm::raw_fd_ostream outLLVMIR("out/test.ll", errorCode);
      ctx->llvm_module.print(outLLVMIR, nullptr);

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

    // malloc(i64) -> ptr
    FunctionType* mallocFunctionType = FunctionType::get(
      context->builder.getPtrTy(), {context->builder.getInt64Ty()}, false);
    context->llvm_module.getOrInsertFunction("malloc", mallocFunctionType);
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

    // FIXME: native functions use an internal name which must not be shadowed
    // by the user. Need to use a name that is not allowable in the language.
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
