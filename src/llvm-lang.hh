#pragma once
#include <trieste/trieste.h>

namespace llvmir {
  using namespace trieste;

  // Program
  inline const auto Program =
    TokenDef("program_llvm", flag::symtab | flag::defbeforeuse);

  // --- Types ---
  inline const auto TypeList = TokenDef("ir_type_list_llvm");
  inline const auto Type = TokenDef("type_llvm");
  inline const auto IRType = TokenDef("ir_type_llvm");
  inline const auto TypeArrow = TokenDef("type_arrow_llvm");

  inline const auto TVoid = TokenDef("void_llvm");
  inline const auto Ti1 = TokenDef("i1_llvm");
  inline const auto Ti32 = TokenDef("i32_llvm");
  inline const auto Ti64 = TokenDef("i64_llvm");
  inline const auto TPtr = TokenDef("pointer_llvm");

  // Arithmetic operators

  // Functions
  inline const auto Fun = TokenDef("fun_llvm", flag::print | flag::symtab);
  inline const auto FunDef =
    TokenDef("fundef_llvm", flag::lookup | flag::shadowing);
  inline const auto ParamList = TokenDef("param_list_llvm");
  inline const auto Param =
    TokenDef("param_llvm", flag::lookup | flag::shadowing);
  inline const auto Body = TokenDef("body_llvm");
  inline const auto Instr = TokenDef("instr_llvm");
  inline const auto Label = TokenDef("label_llvm", flag::print);
  inline const auto Block = TokenDef("basic_block_llvm", flag::print);
  inline const auto Statements = TokenDef("statements_llvm", flag::print);
  inline const auto IRFun =
    TokenDef("ir_function_llvm", flag::print | flag::symtab);

  // Identifiers
  inline const auto Ident = TokenDef("ident_llvm", flag::print);

  // Comparison operations
  inline const auto EQ = TokenDef("eq_llvm");
  inline const auto NE = TokenDef("ne_llvm");
  // Unsigned
  inline const auto UGT = TokenDef("ugt_llvm");
  inline const auto UGE = TokenDef("uge_llvm");
  inline const auto ULT = TokenDef("ult_llvm");
  inline const auto ULE = TokenDef("ule_llvm");
  // Signed
  inline const auto SGT = TokenDef("sgt_llvm");
  inline const auto SGE = TokenDef("sge_llvm");
  inline const auto SLT = TokenDef("slt_llvm");
  inline const auto SLE = TokenDef("sle_llvm");

  // BinaryOps
  inline const auto BinaryOp = TokenDef("binary_op_llvm");
  inline const auto Add = TokenDef("add_llvm");
  inline const auto Sub = TokenDef("sub_llvm");
  inline const auto Mul = TokenDef("mul_llvm");

  // MemoryOps
  inline const auto MemoryOp = TokenDef("memory_op_llvm");
  inline const auto Alloca = TokenDef("alloca_llvm");
  inline const auto Load = TokenDef("load_llvm");
  inline const auto Store = TokenDef("store_llvm");
  inline const auto GetElementPtr = TokenDef("get_element_ptr_llvm");

  // TerminatorOps
  inline const auto Terminator = TokenDef("terminator_llvm");
  inline const auto TerminatorOp = TokenDef("terminator_op_llvm");
  inline const auto Branch = TokenDef("branch_llvm");
  inline const auto Jump = TokenDef("jump_llvm");
  inline const auto Ret = TokenDef("return_llvm");

  // MiscOps
  inline const auto MiscOp = TokenDef("misc_op_llvm");
  inline const auto Call = TokenDef("call_llvm");
  inline const auto CallOpaque = TokenDef("call_opaque_llvm");
  inline const auto Icmp = TokenDef("icmp_llvm");
  inline const auto Phi = TokenDef("phi_llvm");

  // ConversionOps
  inline const auto ConversionOp = TokenDef("conversion_op_llvm");
  inline const auto BitCast = TokenDef("bitcast_llvm");

  // LLVM IR API actions (not LLVM IR instructions)
  inline const auto Action = TokenDef("action_llvm");
  inline const auto CreateConst = TokenDef("create_const_llvm");
  inline const auto CreateStructType = TokenDef("create_struct_type_llvm");
  inline const auto CreateFunType = TokenDef("create_function_type_llvm");
  inline const auto GetFunction = TokenDef("get_function_llvm");
  inline const auto GetType = TokenDef("get_type_llvm");
  inline const auto GetSizeOfType = TokenDef("get_sizeof_type_llvm");

  // Helpers
  inline const auto PredecessorList = TokenDef("predecessor_list_llvm");
  inline const auto Predecessor = TokenDef("predecessor_llvm");
  inline const auto IRValue = TokenDef("value_llvm", flag::print);
  inline const auto PropagateCompile = TokenDef("propagate_compile_llvm");
  inline const auto ArgList = TokenDef("argument_list_llvm");
  inline const auto OffsetList = TokenDef("offset_list_llvm");
  inline const auto Offset = TokenDef("offset_llvm");

  // Convenience tokens
  inline const auto Name = TokenDef("name_llvm", flag::print);
  inline const auto Op = TokenDef("op_llvm");
  inline const auto Lhs = TokenDef("lhs_llvm");
  inline const auto Rhs = TokenDef("rhs_llvm");
  inline const auto Ty1 = TokenDef("ty1_llvm");
  inline const auto Ty2 = TokenDef("ty2_llvm");
  inline const auto TODO = TokenDef("TODO_llvm:", flag::print);
  inline const auto Src = TokenDef("src_llvm");
  inline const auto Dst = TokenDef("dst_llvm");
  inline const auto Result = TokenDef("result_llvm");
  inline const auto True = TokenDef("true_llvm");
  inline const auto False = TokenDef("false_llvm");
  inline const auto Cond = TokenDef("cond_llvm");

  /* -- Well-formedness specification -- */
  inline const auto wf_comparison =
    (EQ | NE | UGT | UGE | ULT | ULE | SGT | SGE | SLT | SLE);
  inline const auto wf_llvm_types = (Ti1 | Ti32 | Ti64 | TPtr | TypeArrow);

  // clang-format off
  inline const auto wf =
    (Top <<= Program)
    | (Program <<= (Action | IRFun)++[1])
    | (IRFun <<= TypeArrow * ParamList * Body)
      | (TypeArrow <<= (Ty1 >>= wf_llvm_types) * (Ty2 >>= wf_llvm_types))
      | (ParamList <<= Param++)
        | (Param <<= Ident * (Type >>= wf_llvm_types))
      | (Body <<= (Block)++[1])
        | (Block <<= (Label * Statements * Terminator))
         | (Statements <<= (Instr | Action)++)
         | (Terminator <<= TerminatorOp)
    // Builder actions, which aren't LLVM IR instructions.
    | (Action <<= (CreateConst | CreateStructType | CreateFunType | GetFunction | GetType | GetSizeOfType))
      | (CreateConst <<= Ident * (Type >>= Ti64 | Ti32 | Ti1) * IRValue)
      | (CreateStructType <<= Ident * TypeList)
        | (TypeList <<= wf_llvm_types++)
      | (CreateFunType <<= (Result >>= Ident) * (Type >>= wf_llvm_types) * TypeList)
        | (TypeList <<= wf_llvm_types++)
      | (GetFunction <<= (Result >>= Ident) * (Fun >>= Ident))
      | (GetType <<= (Result >>= Ident) * (Type >>= Ident))
      | (GetSizeOfType <<= (Result >>= Ident) * (Type >>= (Ti32 | Ti64)) * (IRType >>= Ident))
    // LLVM IR instructions.
    | (Instr <<= (BinaryOp | MemoryOp | MiscOp | ConversionOp))
    | (BinaryOp <<= (Add | Sub | Mul))
      | (Add <<= (Result >>= Ident) * (Type >>= Ti32) * (Lhs >>= Ident) * (Rhs >>= Ident))
      | (Sub <<= (Result >>= Ident) * (Type >>= Ti32) * (Lhs >>= Ident) * (Rhs >>= Ident))
      | (Mul <<= (Result >>= Ident) * (Type >>= Ti32) * (Lhs >>= Ident) * (Rhs >>= Ident))
    | (MemoryOp <<= (Alloca | Load | Store | GetElementPtr))
      | (Alloca <<= (Result >>= Ident) * (Type >>= wf_llvm_types))
      | (Load <<= (Result >>= Ident) * (Type >>= wf_llvm_types) * (Src >>= Ident))
      | (Store <<= (IRValue >>= Ident) * (Dst >>= Ident))
      | (GetElementPtr <<= (Result >>= Ident) * (IRType >>= Ident) * (IRValue >>= Ident) * OffsetList)
        | (OffsetList <<= Offset++)
          | (Offset <<= (IRType >>= wf_llvm_types) * IRValue)
    | (MiscOp <<= (Call | CallOpaque | Icmp | Phi))
      | (Call <<= ((Result >>= Ident) * (Fun >>= Ident) * ArgList))
      | (CallOpaque <<= ((Result >>= Ident) * (IRType >>= Ident) * (Fun >>= Ident) * ArgList))
        | (ArgList <<= (Ident)++)
      | (Icmp <<= (Result >>= Ident) * (Op >>= wf_comparison) * (Type >>= (Ti1 | Ti32)) * (Lhs >>= Ident) * (Rhs >>= Ident))
      | (Phi <<= (Result >>= Ident) * (Type >>= wf_llvm_types) * PredecessorList)
        | (PredecessorList <<= Predecessor++)
          | (Predecessor <<= (IRValue >>= Ident) * Label)
    | (ConversionOp <<= (BitCast))
      | (BitCast <<= (Result >>= Ident) * (IRValue >>= Ident) * (IRType >>= (Ident | wf_llvm_types)))
    | (TerminatorOp <<= (Branch | Jump | Ret))
      | (Branch <<= (Cond >>= Ident) * (True >>= Label) * (False >>= Label))
      | (Jump <<= (Label))
      | (Ret <<= Ident)
    ;
  // clang-format on
}
