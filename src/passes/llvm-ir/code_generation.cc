#include "../../llvm-lang.hh"
#include "../../runtime.hh"
#include "../llvm_utils.hh"
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

namespace llvmir {

  using namespace trieste;
  using namespace llvm;

  /**
   * @brief
   * This pass lowers to LLVM IR code.
   */
  PassDef
  code_generation(std::string input_filepath, std::string output_filepath) {
    auto ctx = std::make_shared<LLVMIRContext>(input_filepath, output_filepath);

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
    PassDef pass =
      {
        "generateLLVMIR",
        llvmir::wf,
        dir::topdown | dir::once,
        {
          /**
           * Binary Operations
           */
          T(Instr)
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

            return NoChange;
          },

          /**
           * Memory Operations
           */
          // Alloca
          T(Instr)
              << (T(MemoryOp)
                  << (T(Alloca)
                      << T(Ident)[Ident] * (T(Ti64, Ti32, Ti1, TPtr)[Type]))) >>
            [ctx](Match& _) -> Node {
            std::string resultId = node_val(_(Ident));

            llvm::Type* llvmType = createLLVMType(ctx, _(Type));
            assert(llvmType);

            AllocaInst* result =
              ctx->builder.CreateAlloca(llvmType, nullptr, resultId);

            ctx->registers[resultId] = result;

            return NoChange;
          },

          // Store
          T(Instr)
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

            return NoChange;
          },

          // Load
          T(Instr)
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

            return NoChange;
          },

          // GetElementPointer
          T(Instr)
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
            for (Node offsetNode : *_(OffsetList)) {
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

            return NoChange;
          },

          /**
           * Misc. Operations
           */
          // Function Call
          T(Instr)
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
            for (Node arg : *_(ArgList)) {
              std::string argId = node_val(arg);
              Value* argVal = ctx->registers[argId];
              assert(argVal);

              arguments.push_back(argVal);
            }

            std::string resultId = node_val(_(Result));
            Value* result =
              ctx->builder.CreateCall(function, arguments, resultId);

            ctx->registers[resultId] = result;

            return NoChange;
          },

          // Function Call (Opaque)
          T(Instr)
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
            for (Node arg : *_(ArgList)) {
              std::string argId = node_val(arg);
              Value* argVal = ctx->registers[argId];
              assert(argVal);

              arguments.push_back(argVal);
            }

            std::string resultId = node_val(_(Result));
            Value* result =
              ctx->builder.CreateCall(funTy, functionPtr, arguments, resultId);

            ctx->registers[resultId] = result;

            return NoChange;
          },

          // Compare
          T(Instr)
              << (T(MiscOp)
                  << (T(Icmp) << T(Ident)[Ident] * T(EQ, ULT, SLT)[Op] *
                        T(Ti32, Ti1)[Type] * T(Ident)[Lhs] * T(Ident)[Rhs])) >>
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
            }
            assert(result && "Icmp - unexpected comparison operator");

            ctx->registers[resultId] = result;

            return NoChange;
          },

          // Phi
          T(Instr)
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

            return NoChange;
          },

          // Label
          In(Block) * T(Label)[Label] >> [ctx](Match& _) -> Node {
            std::string funId = node_val(_(Label)->parent(IRFun));
            std::string blockId = node_val(_(Label));

            llvm::BasicBlock* block = ctx->basicBlocks[blockId];
            assert(block);

            ctx->builder.SetInsertPoint(block);

            return NoChange;
          },

          /**
           * Terminating Operations.
           */
          // Branch
          T(TerminatorOp)
              << (T(Branch)
                  << (T(Ident)[Cond] * T(Label)[True] * T(Label)[False])) >>
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

            return NoChange;
          },

          // Jump
          T(TerminatorOp) << (T(Jump) << (T(Label)[Label])) >>
            [ctx](Match& _) -> Node {
            std::string blockId = node_val(_(Label));
            llvm::BasicBlock* block = ctx->basicBlocks[blockId];
            assert(block);

            ctx->builder.CreateBr(block);

            return NoChange;
          },

          // Return
          T(TerminatorOp) << (T(Ret) << T(Ident)[Ident]) >>
            [ctx](Match& _) -> Node {
            std::string resultId = node_val(_(Ident));
            Value* result = ctx->registers[resultId];
            assert(result);

            ctx->builder.CreateRet(result);

            return NoChange;
          },

          /**
           * Conversion Operations
           */
          // BitCast (Custom Type)
          T(Instr)
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
            Value* result =
              ctx->builder.CreateBitCast(valueToConvert, targetType, resultId);
            ctx->registers[resultId] = result;

            return NoChange;
          },

          // BitCast (Fixed Type)
          T(Instr)
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
            Value* result =
              ctx->builder.CreateBitCast(valueToConvert, targetType, resultId);
            ctx->registers[resultId] = result;

            return NoChange;
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

            for (Node param : *_(ParamList)) {
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

            return NoChange;
          },

          /**
           * Actions performed by LLVM IR builder.
           */
          // Get Type
          T(Action) << (T(GetType) << T(Ident)[Result] * T(Ident)[IRType]) >>
            [ctx](Match& _) -> Node {
            std::string valueId = node_val(_(IRType));
            llvm::Value* value = ctx->registers[valueId];
            assert(value);

            llvm::Type* destType = value->getType();

            std::string resultId = node_val(_(Result));
            ctx->types[resultId] = destType;

            return NoChange;
          },

          // Get Function
          T(Action) << (T(GetFunction) << T(Ident)[Result] * T(Ident)[Fun]) >>
            [ctx](Match& _) -> Node {
            std::string tmpIdent = node_val(_(Result));
            std::string functionName = node_val(_(Fun));
            Function* theFunction = ctx->llvm_module.getFunction(functionName);
            assert(theFunction);

            ctx->registers[tmpIdent] = theFunction;

            return NoChange;
          },

          // Create Struct Type
          T(Action)
              << (T(CreateStructType)
                  << T(Ident)[Result] * T(TypeList)[TypeList]) >>
            [ctx](Match& _) -> Node {
            std::vector<llvm::Type*> fieldTypes;
            for (Node irType : *_(TypeList)) {
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

            return NoChange;
          },

          // Create Function Type
          T(Action)
              << (T(CreateFunType) << T(Ident)[Result] *
                    T(Ti1, Ti32, Ti64, TPtr)[IRType] *
                    T(TypeList)[ParamList]) >>
            [ctx](Match& _) -> Node {
            Node returnType = _(IRType);
            llvm::Type* returnLLVMType = createLLVMType(ctx, returnType);
            assert(returnLLVMType);

            std::vector<llvm::Type*> paramTypes;
            for (Node paramTy : *_(ParamList)) {
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

            return NoChange;
          },

          // Create Constant
          T(Action)
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

            return NoChange;
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

            auto dl = llvm::DataLayout(ctx->llvm_module.getDataLayout());
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

            return NoChange;
          },

        }};

    pass.pre([ctx](Node) {
      genExternalFunctions(ctx);
      genRuntimeFunctions(ctx);

      return 0;
    });

    pass.pre(IRFun, [ctx](Node functionToken) {
      ctx->registers.clear();
      std::string funId = node_val(functionToken);
      llvm::Function* function = ctx->llvm_module.getFunction(funId);

      Argument* arg = function->arg_begin();
      Node paramList = functionToken / ParamList;
      for (Node param : *paramList) {
        std::string paramName = node_val(param / Ident);
        arg->setName(paramName);
        ctx->registers[paramName] = arg;
        arg++;
      }

      return 0;
    });

    pass.post(IRFun, [ctx](Node functionToken) {
      std::string fun_name = node_val(functionToken);
      Function* fun = ctx->llvm_module.getFunction(fun_name);
      verifyFunction(*fun, &llvm::errs());

      return 0;
    });

    pass.post([ctx](Node) {
      Function* main = ctx->llvm_module.getFunction("main");
      verifyFunction(*main, &llvm::errs());
      verifyModule(ctx->llvm_module, &llvm::errs());

      std::string outfile;
      if (!ctx->output_file.empty()) {
        std::filesystem::path filepath =
          std::filesystem::absolute(ctx->output_file);
        outfile = filepath.replace_extension(".ll").string();
      } else {
        outfile = "./out.ll";
      }
      std::error_code errorCode;
      llvm::raw_fd_ostream outLLVMIR(outfile, errorCode);
      ctx->llvm_module.print(outLLVMIR, nullptr);

      return 0;
    });

    return pass;
  }

}
