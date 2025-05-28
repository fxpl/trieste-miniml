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

#include <llvm/IR/DerivedTypes.h>
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
    // LLVM Code generation APIs:
    // LLVMContext - Holds core data structures, Type and constant value tables.
    // IRBuilder - Generates LLVM IR instructions.
    // Module - Contains generated instructions, local and global value tables.
    llvm::LLVMContext llvm_context;
    // FIXME: NoFolder is used to prevent constant folding.
    llvm::IRBuilder<NoFolder> builder;
    llvm::Module llvm_module;

    // Keeps track of generated BasicBlocks within program
    std::map<std::string, llvm::BasicBlock*> basicBlocks;
    // Keeps track of generated LLVM types within program
    std::map<std::string, llvm::Type*> types;

    // Keeps track of generated LLVM values within a function
    std::map<std::string, llvm::Value*> registers;

    LLVMIRContext()
    : builder(llvm_context), llvm_module("miniML", llvm_context) {}

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
    auto context = std::make_shared<LLVMIRContext>();

    // FIXME: Debug print
    std::cout << "## CodeGen Pass ##" << std::endl;

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

              // FIXME: Debug print
              std::cout << "BinaryOp - Add/Sub/Mul" << std::endl;

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
              [context](Match& _) -> Node {
              std::string resultId = node_val(_(Ident));

              llvm::Type* llvmType = NULL;
              if (_(Type) == Ti32) {
                llvmType = context->builder.getInt32Ty();
              } else if (_(Type) == Ti1) {
                llvmType = context->builder.getInt1Ty();
              } else if (_(Type) == Ti64) {
                llvmType = context->builder.getInt64Ty();
              } else if (_(Type) == TPtr) {
                llvmType = context->builder.getPtrTy();
              }
              assert(llvmType);

              AllocaInst* result =
                context->builder.CreateAlloca(llvmType, nullptr, resultId);

              context->registers[resultId] = result;

              // FIXME: Debug print
              std::cout << "MemoryOp - Alloca" << std::endl;

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

              // FIXME: Debug print
              std::cout << "MemoryOp - Store" << std::endl;

              return _(Instr);
            },

            // Load
            T(Instr)[Instr]
                << (T(MemoryOp)
                    << (T(Load) << T(Ident)[Ident] * T(Ti32, Ti1, TPtr)[Type] *
                          T(Ident)[Src])) >>
              [context](Match& _) -> Node {
              std::string resultId = node_val(_(Ident));

              llvm::Type* type = NULL;
              if (_(Type) == Ti32) {
                type = context->builder.getInt32Ty();
              } else if (_(Type) == Ti1) {
                type = context->builder.getInt1Ty();
              } else if (_(Type) == TPtr) {
                type = context->builder.getPtrTy();
              }
              assert(type);

              Value* src = context->registers[node_val(_(Src))];
              assert(src);

              Value* result = context->builder.CreateLoad(type, src, resultId);

              context->registers[resultId] = result;

              // FIXME: Debug print
              std::cout << "MemoryOp - Load" << std::endl;

              return _(Instr);
            },

            // GetElementPointer
            T(Instr)[Instr]
                << (T(MemoryOp)
                    << (T(GetElementPtr)
                        << (T(Ident)[Result] * T(Ident)[IRType] *
                            T(Ident)[IRValue] * T(OffsetList)[OffsetList]))) >>
              [context](Match& _) -> Node {
              std::string valueId = node_val(_(IRValue));
              Value* value = context->registers[valueId];
              assert(value);

              std::string typeId = node_val(_(IRType));
              llvm::Type* type = context->types[typeId];
              assert(type);

              std::vector<Value*> offsets;
              for (size_t i = 0; i < _(OffsetList)->size(); i++) {
                Node offsetNode = _(OffsetList)->at(i);

                std::string valStr = node_val(offsetNode / IRValue);
                Node offsetType = offsetNode / IRType;
                Value* offset = nullptr;
                if (offsetType == Ti32) {
                  int offsetValue = stoi(valStr);
                  offset = context->builder.getInt32(offsetValue);
                }
                assert(offset);

                offsets.push_back(offset);
              }

              std::string resultId = node_val(_(Result));
              Value* result =
                context->builder.CreateGEP(type, value, offsets, resultId);
              context->registers[resultId] = result;

              // FIXME: Debug print
              std::cout << "MemoryOp - GEP" << std::endl;

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
              [context](Match& _) -> Node {
              std::string funId = node_val(_(Fun));
              Function* function = context->llvm_module.getFunction(funId);

              if (function == nullptr) {
                function = (Function*)context->registers[funId];
              }
              assert(function);

              std::vector<llvm::Value*> arguments;
              for (size_t i = 0; i < _(ArgList)->size(); i++) {
                Node arg = _(ArgList)->at(i);
                std::string argId = node_val(arg);
                Value* argVal = context->registers[argId];
                assert(argVal);

                arguments.push_back(argVal);
              }

              std::string resultId = node_val(_(Result));
              Value* result =
                context->builder.CreateCall(function, arguments, resultId);

              context->registers[resultId] = result;

              // FIXME: Debug print
              std::cout << "MiscOp - Call " << funId << std::endl;

              return _(Instr);
            },

            // Function Call (Opaque)
            T(Instr)[Instr]
                << (T(MiscOp)
                    << (T(CallOpaque) << T(Ident)[Result] * T(Ident)[IRType] *
                          T(Ident)[Fun] * T(ArgList)[ArgList])) >>
              [context](Match& _) -> Node {
              std::string funTyId = node_val(_(IRType));
              llvm::FunctionType* funTy =
                (llvm::FunctionType*)context->types[funTyId];
              assert(funTy);

              std::string funId = node_val(_(Fun));
              llvm::Value* functionPtr = context->registers[funId];
              assert(functionPtr);

              std::vector<llvm::Value*> arguments;
              for (size_t i = 0; i < _(ArgList)->size(); i++) {
                Node arg = _(ArgList)->at(i);
                std::string argId = node_val(arg);
                Value* argVal = context->registers[argId];
                assert(argVal);

                arguments.push_back(argVal);
              }

              std::string resultId = node_val(_(Result));
              Value* result = context->builder.CreateCall(
                funTy, functionPtr, arguments, resultId);

              context->registers[resultId] = result;

              // FIXME: Debug print
              std::cout << "MiscOp - Call (Opaque)" << funId << std::endl;

              return _(Instr);
            },

            // Compare
            T(Instr)[Instr]
                << (T(MiscOp)
                    << (T(Icmp) << T(Ident)[Ident] * T(EQ, ULT)[Op] *
                          T(Ti32, Ti1)[Type] * T(Ident)[Lhs] *
                          T(Ident)[Rhs])) >>
              [context](Match& _) -> Node {
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

              // FIXME: Debug print
              std::cout << "MiscOp - Compare" << std::endl;

              return _(Instr);
            },

            // Phi
            T(Instr)[Instr]
                << (T(MiscOp)
                    << (T(Phi) << T(Ident)[Ident] * T(Ti32, Ti1)[Type] *
                          (T(PredecessorList)
                           << T(Predecessor)[True] * T(Predecessor)[False]))) >>
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

              // FIXME: Debug print
              std::cout << "MiscOp - Phi" << std::endl;

              return _(Instr);
            },

            // Label
            In(Block) * T(Label)[Label] >> [context](Match& _) -> Node {
              std::string funId = node_val(_(Label)->parent(IRFun));
              std::string blockId = node_val(_(Label));

              // FIXME: Debug print
              std::cout << "Label - Setting cursor at: " << blockId << " in "
                        << funId << std::endl;

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
                    << (T(Branch)
                        << (T(Ident)[Cond] * T(Label)[True] *
                            T(Label)[False]))) >>
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

              // FIXME: Debug print
              std::cout << "TerminatorOp - Branch to blocks: " << trueId
                        << " or " << falseId << std::endl;

              return _(Instr);
            },

            // Jump
            T(Instr)[Instr]
                << (T(TerminatorOp) << (T(Jump) << (T(Label)[Label]))) >>
              [context](Match& _) -> Node {
              std::string blockId = node_val(_(Label));
              llvm::BasicBlock* block = context->basicBlocks[blockId];
              assert(block);

              context->builder.CreateBr(block);

              // FIXME: Debug print
              std::cout << "TerminatorOp - Jump to block: " << blockId
                        << std::endl;

              return _(Instr);
            },

            // Return
            T(Instr)[Instr]
                << (T(TerminatorOp) << (T(Ret) << T(Ident)[Ident])) >>
              [context](Match& _) -> Node {
              std::string resultId = node_val(_(Ident));
              Value* result = context->registers[resultId];
              assert(result);

              context->builder.CreateRet(result);

              // FIXME: Debug print
              std::cout << "TerminatorOp - Return" << std::endl;

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
              [context](Match& _) -> Node {
              std::string targetTypeId = node_val(_(IRType));
              llvm::Type* targetType = context->types[targetTypeId];
              assert(targetType);

              std::string valueToConvertId = node_val(_(IRValue));
              Value* valueToConvert = context->registers[valueToConvertId];
              assert(valueToConvert);

              std::string resultId = node_val(_(Result));
              Value* result = context->builder.CreateBitCast(
                valueToConvert, targetType, resultId);
              context->registers[resultId] = result;

              // FIXME: Debug print
              std::cout << "ConversionOp - BitCast" << std::endl;

              return _(Instr);
            },

            // BitCast (Fixed Type)
            T(Instr)[Instr]
                << (T(ConversionOp)
                    << (T(BitCast) << T(Ident)[Result] * T(Ident)[IRValue] *
                          T(Ti1, Ti32, Ti64, TPtr)[IRType])) >>
              [context](Match& _) -> Node {
              llvm::Type* targetType = nullptr;
              if (_(IRType) == Ti1) {
                targetType = context->builder.getInt1Ty();
              } else if (_(IRType) == Ti32) {
                targetType = context->builder.getInt32Ty();
              } else if (_(IRType) == Ti64) {
                targetType = context->builder.getInt64Ty();
              } else if (_(IRType) == TPtr) {
                targetType = context->builder.getPtrTy();
              }
              assert(targetType);

              std::string valueToConvertId = node_val(_(IRValue));
              Value* valueToConvert = context->registers[valueToConvertId];
              assert(valueToConvert);

              std::string resultId = node_val(_(Result));
              Value* result = context->builder.CreateBitCast(
                valueToConvert, targetType, resultId);
              context->registers[resultId] = result;

              // FIXME: debug print
              std::cout << "ConversionOp - BitCast" << std::endl;

              return _(Instr);
            },

            /**
             * Function Declaration
             */
            T(IRFun)[Fun] << T(TypeArrow)[TypeArrow] * T(ParamList)[ParamList] *
                  T(Body) >>
              [context](Match& _) -> Node {
              // FIXME: Debug print
              std::cout << "Fun - declaring fun: " << node_val(_(Fun))
                        << std::endl;

              // Create function type.
              Node returnType = _(TypeArrow) / Ty2;
              llvm::Type* returnLLVMType = NULL;
              if (returnType == Ti32) {
                returnLLVMType = context->builder.getInt32Ty();
              } else if (returnType == Ti1) {
                returnLLVMType = context->builder.getInt1Ty();
              } else if (returnType == TPtr) {
                returnLLVMType = context->builder.getPtrTy();
              }
              assert(returnLLVMType);

              std::vector<llvm::Type*> paramTypes;
              for (size_t i = 0; i < _(ParamList)->size(); i++) {
                Node param = _(ParamList)->at(i);
                Node type = param / Type;
                llvm::Type* llvmType = nullptr;
                if (type == Ti32) {
                  llvmType = context->builder.getInt32Ty();
                } else if (type == Ti1) {
                  llvmType = context->builder.getInt1Ty();
                } else if (type == TPtr) {
                  llvmType = context->builder.getPtrTy();
                }
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
                context->llvm_module);
              theFunction->setCallingConv(CallingConv::C);
              context->registers[theFunctionName] = theFunction;

              return _(Fun);
            },

            /**
             * Actions performed by LLVM IR builder.
             */
            // Get Type
            T(Action)[Action]
                << (T(GetType) << T(Ident)[Result] * T(Ident)[IRType]) >>
              [context](Match& _) -> Node {
              std::string valueId = node_val(_(IRType));
              llvm::Value* value = context->registers[valueId];
              assert(value);

              llvm::Type* destType = value->getType();

              std::string resultId = node_val(_(Result));
              context->types[resultId] = destType;

              // FIXME: Debug print.
              std::cout << "Action - GetType" << std::endl;

              return _(Action);
            },

            // Get Function
            T(Action)[Action]
                << (T(GetFunction) << T(Ident)[Result] * T(Ident)[Fun]) >>
              [context](Match& _) -> Node {
              std::string tmpIdent = node_val(_(Result));
              std::string functionName = node_val(_(Fun));
              Function* theFunction =
                context->llvm_module.getFunction(functionName);
              assert(theFunction);

              context->registers[tmpIdent] = theFunction;

              // FIXME: Debug print.
              std::cout << "Action - Storing fun: " << functionName
                        << " as: " << tmpIdent << std::endl;

              return _(Action);
            },

            // Create Struct Type
            T(Action)[Action]
                << (T(CreateStructType)
                    << T(Ident)[Result] * T(IRTypeList)[IRTypeList]) >>
              [context](Match& _) -> Node {
              std::vector<llvm::Type*> fieldTypes;
              for (auto irType : *_(IRTypeList)) {
                llvm::Type* llvmType = nullptr;
                if (irType == Ti32) {
                  llvmType = context->builder.getInt32Ty();
                } else if (irType == Ti1) {
                  llvmType = context->builder.getInt1Ty();
                } else if (irType == TPtr) {
                  llvmType = context->builder.getPtrTy();
                }
                assert(llvmType);

                fieldTypes.push_back(llvmType);
              }

              std::string resultId = node_val(_(Result));
              llvm::StructType* theStructType =
                llvm::StructType::create(context->llvm_context, resultId);
              theStructType->setBody(fieldTypes, false);
              assert(theStructType);

              context->types[resultId] = theStructType;

              // FIXME: Debug print.
              std::cout << "Action - Creating struct type: " << resultId
                        << std::endl;

              return _(Action);
            },

            // Create Function Type
            T(Action)[Action]
                << (T(CreateFunType) << T(Ident)[Result] *
                      T(Ti1, Ti32, Ti64, TPtr)[IRType] *
                      T(IRTypeList)[ParamList]) >>
              [context](Match& _) -> Node {
              Node returnType = _(IRType);
              llvm::Type* returnLLVMType = NULL;
              if (returnType == Ti32) {
                returnLLVMType = context->builder.getInt32Ty();
              } else if (returnType == Ti1) {
                returnLLVMType = context->builder.getInt1Ty();
              } else if (returnType == TPtr) {
                returnLLVMType = context->builder.getPtrTy();
              }
              assert(returnLLVMType);

              std::vector<llvm::Type*> paramTypes;
              for (size_t i = 0; i < _(ParamList)->size(); i++) {
                Node paramTy = _(ParamList)->at(i);
                llvm::Type* llvmType = nullptr;
                if (paramTy == Ti32) {
                  llvmType = context->builder.getInt32Ty();
                } else if (paramTy == Ti1) {
                  llvmType = context->builder.getInt1Ty();
                } else if (paramTy == TPtr) {
                  llvmType = context->builder.getPtrTy();
                }
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
              context->types[resultId] = theFunctionType;

              // FIXME: Debug print
              std::cout << "Action - Creating funType at: " << resultId
                        << std::endl;

              return _(Action);
            },

            // Create Constant
            T(Action)[Action]
                << (T(CreateConst)
                    << (T(Ident)[Ident] * T(Ti64, Ti32, Ti1)[Type] *
                        T(IRValue)[IRValue])) >>
              [context](Match& _) -> Node {
              std::string regId = node_val(_(Ident));
              std::string valueStr = node_val(_(IRValue));

              Value* value = NULL;
              if (_(Type) == Ti32) {
                value = context->builder.getInt32(std::stoi(valueStr));
              } else if (_(Type) == Ti1) {
                value = context->builder.getInt1(std::stoi(valueStr));
              } else if (_(Type) == Ti64) {
                value = context->builder.getInt64(std::stoi(valueStr));
              }
              assert(value);

              context->registers[regId] = value;

              // FIXME: Debug print
              std::cout << "Action - Creating value: " << valueStr
                        << " at: " << regId << "\n";

              return _(Action);
            },

            // Create Basic Block
            In(Body) * T(Block)[Block] >> [context](Match& _) -> Node {
              std::string funName = node_val(_(Block)->parent(IRFun));
              Function* function = context->llvm_module.getFunction(funName);
              assert(function);

              // FIXME: Debug print
              std::cout << "Block - Creating block: " << node_val(_(Block))
                        << " in: " << funName << "\n";

              std::string blockId = node_val(_(Block));
              BasicBlock* block =
                BasicBlock::Create(context->llvm_context, blockId, function);

              context->basicBlocks[blockId] = block;

              return _(Block);
            },

          }};

    pass.pre([context](Node) {
      // TODO: Refactor these to separate file to decouple code gen from source
      // language.
      /**
       * External functions
       */
      genExternalFunctions(context);

      /**
       * Internal native functions
       */
      genPrintInt(context);
      genPrintBool(context);

      // Declare a closure type
      StructType* ClosureTy =
        StructType::create(context->llvm_context, "ClosureTy");
      ClosureTy->setBody(
        {context->builder.getPtrTy(), context->builder.getPtrTy()}, false);

      context->types["ClosureTy"] = ClosureTy;

      return 0;
    });

    pass.pre(IRFun, [context](Node functionToken) {
      context->registers.clear();
      std::string funId = node_val(functionToken);
      llvm::Function* function = context->llvm_module.getFunction(funId);

      Argument* arg = function->arg_begin();
      Node paramList = functionToken / ParamList;
      for (size_t i = 0; i < paramList->size(); i++) {
        Node param = paramList->at(i);
        std::string paramName = node_val(param / Ident);
        arg->setName(paramName);
        context->registers[paramName] = arg;
        arg++;
      }

      return 0;
    });

    pass.post([context](Node) {
      // FIXME: Should verify all functions.
      Function* main = context->llvm_module.getFunction("main");
      verifyFunction(*main, &llvm::errs());
      verifyModule(context->llvm_module, &llvm::errs());

      // TODO: Figure out how to output. into file? into clang via API?
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
