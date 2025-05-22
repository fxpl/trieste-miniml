#pragma once
// clang-format off
#include "../miniml-lang.hh"

namespace miniml{
 using namespace trieste;

    std::vector<Pass> passes();
    Parse parser();

    inline const auto wf_types = TInt | TBool | TVar | TypeArrow ;
    inline const auto wf_expr = Int | True | False
    | Add | Sub | Mul | LT | Equals
    | If | Ident | Fun | App | Print;

    namespace init_parse{

    inline const auto wf_parse_tokens =
     Add | Sub | Mul |
     Equals | LT |
     Int | True | False |
     If | Then | Else |
     Ident | Fun | Is | Let |
     Colon | Paren |
     Print |
     wf_types
     ;

    inline const wf::Wellformed wf =
      (Top <<= File)
    | (File <<= ~(Term | Group))
    | (Paren <<= ~Group)
    | (Term <<= Group++[1])
    | (Fun <<= ~Group)
    | (Is <<= ~Group)
    | (If <<= ~Group)
    | (Then <<= ~Group)
    | (Else <<= ~Group)
    | (Group <<= wf_parse_tokens++[1])
    ;

    }
    namespace parse{

    inline const auto wf_parse_cleanup = init_parse::wf - Term
    | (Top <<= Program)
    | (Program <<= (Group)++)
    ;

    inline const auto wf_exp_tokens_fun = init_parse::wf_parse_tokens
        - (wf_types | Is | Let | Colon | Term);

    inline const auto wf_group_tokens = (init_parse::wf_parse_tokens | Expr)
        - (wf_types | Colon | Is | Fun);

    inline const auto wf_fun =
      wf_parse_cleanup
    | (Fun <<= FunDef)
    | (FunDef <<= Ident * Annotation * Param * Expr)
    | (Annotation <<= Type >>= (wf_types | TNone))
    | (TypeArrow <<= (Ty1 >>= wf_types) * (Ty2 >>= wf_types))
    | (Param <<= Ident * Annotation)
    | (Expr <<= (wf_exp_tokens_fun)++[1])
    | (Group <<= (wf_group_tokens)++[1])
    ;

    inline const auto wf_exp_tokens_par = (wf_exp_tokens_fun | Expr) - Paren;

    inline const auto wf_group_tokens_par = (wf_group_tokens | Let) - Paren;

    inline const auto wf_parens =
    wf_fun
    | (Expr <<= wf_exp_tokens_par++[1])
    | (Group <<= wf_group_tokens_par++[1])
    ;

    inline const auto wf_exp_tokens_cond = wf_exp_tokens_par - Then - Else;
    inline const auto wf_group_tokens_cond = wf_group_tokens_par - Then - Else;

    inline const auto wf_conditionals =
      (wf_parens - Then - Else)
    | (If <<= Expr * Expr * Expr)
    | (Expr <<= wf_exp_tokens_cond++[1])
    | (Group <<= wf_group_tokens_cond++[1])
    ;

    inline const auto wf_let =
      wf_conditionals
    | (Program <<= TopExpr++)
    | (TopExpr <<= (Let | Expr))
    | (Let <<= Ident * Expr)
    ;

    inline const auto wf_exp_tokens_app =
      wf_exp_tokens_cond | App;

    inline const auto wf_funapp =
      wf_let
    | (Expr <<= wf_exp_tokens_app++[1])
    | (App <<= (Lhs >>= Expr) * (Rhs >>= Expr))
    ;

    inline const auto wf_mul =
      wf_funapp
    | (Mul <<= (Lhs >>= Expr) * (Rhs >>= Expr))
    ;

    inline const auto wf_add =
      wf_mul
    | (Add <<= (Lhs >>= Expr) * (Rhs >>= Expr))
    | (Sub <<= (Lhs >>= Expr) * (Rhs >>= Expr))
    ;

    inline const auto wf_cmp =
      wf_add
    | (LT <<= (Lhs >>= Expr) * (Rhs >>= Expr))
    | (Equals <<= (Lhs >>= Expr) * (Rhs >>= Expr))
    ;

    // Final WF
    inline const wf::Wellformed wf =
    (Top <<= Program)
    | (Program <<= TopExpr++)
    | (TopExpr <<= (Let | Expr))
    | (Let <<= Ident * Expr)
    | (Expr <<= wf_expr)
    | (If <<= Expr * Expr * Expr)
    | (Fun <<= FunDef)
    | (FunDef <<= Ident * Annotation * Param * Expr)[Ident]
    | (Param <<= Ident * Annotation)
    | (App <<= (Lhs >>= Expr) * (Rhs >>= Expr))
    | (Mul <<= (Lhs >>= Expr) * (Rhs >>= Expr))
    | (Add <<= (Lhs >>= Expr) * (Rhs >>= Expr))
    | (Sub <<= (Lhs >>= Expr) * (Rhs >>= Expr))
    | (LT <<= (Lhs >>= Expr) * (Rhs >>= Expr))
    | (Equals <<= (Lhs >>= Expr) * (Rhs >>= Expr))
    | (Annotation <<= Type >>= (wf_types | TNone))
    | (TypeArrow <<= (Ty1 >>= wf_types) * (Ty2 >>= wf_types))
    ;

    }

    namespace check{

    inline const auto wf_fresh = parse::wf
    | (TopExpr <<= Constraints * (Expr >>= (Let | Expr)))
    | (Constraints <<= EqConstr++)
    | (EqConstr <<= (Ty1 >>= wf_types) * (Ty2 >>= wf_types))
    | (Param <<= Ident * Type)[Ident] // bind fun arguments to symtab
    | (Let <<= Ident * Type * Expr)[Ident] // bind let variable to symtab
    | (Type <<= (Type >>= wf_types))
    | (FunDef <<= Ident * Type * Param * Expr)[Ident] // to access nested functions
    ;

    inline const auto wf_inf_exprs = wf_fresh
    | (Constraints <<= (EqConstr | InstConstr | GenConstr)++)
    | (GenConstr <<= (Ty1 >>= TVar) * (Ty2 >>= wf_types)) // Ty1 is a generalization of Ty2
    | (InstConstr <<= (Ty1 >>= TVar) * (Ty2 >>= TVar)) // Ty1 is an instance of Ty2
    | (Expr <<= Type * (Expr >>= wf_expr))
    ;

    inline const auto wf_solve_constr =
    (wf_inf_exprs - Constraints - EqConstr - GenConstr - InstConstr)
    | (Type <<= (Type >>= wf_types | ForAllTy))
    | (ForAllTy <<= TVars * Type)
    | (TVars <<= TVar++)
    ;

    // Final Wf
    inline const auto wf =
    parse::wf
    | (Type <<= (Type >>= wf_types | ForAllTy))
    | (ForAllTy <<= TVars * Type)
    | (TVars <<= TVar++)
    | (Let <<= Ident * Type * Expr)[Ident]
    | (Expr <<= Type * (Expr >>= wf_expr))
    | (Param <<= Ident * Type)[Ident]
    | (FunDef <<= Ident * Type * Param * Expr)[Ident]
    ;

    }

    namespace closures{
    
      inline const auto wf_freeVars = 
        check::wf
        | (FunDef <<= Ident * Type * Param * Expr * FreeVarList)[Ident]
          | (FreeVarList <<= FreeVar++)
            | (FreeVar <<= (Ident >>= (Ident | Global)) * Type)
        | (Expr <<= Type * (Expr >>= (wf_expr | Global)))
        ;

      inline const auto wf_functions =
        wf_freeVars
        | (Program <<= IRFun++)
        | (IRFun <<= Ident * Type * ParamList * Env * Body)
        | (ParamList <<= Param++)
        | (Body <<= TopExpr++)
        ;

      
      inline const auto wf =
        wf_functions
        // Remove symbol table references
        | (FunDef <<= Ident * Type * Param * Expr * FreeVarList)
        | (Param <<= Ident * Type)
        | (Let <<= Ident * Type * Expr)
        // WF
        | (Program <<= (Env | IRFun)++[1])
        | (Env <<= Type++)
        | (Body <<= (TopExpr | Expr)++)
        | (TopExpr <<= (Let | Expr))
        | (Expr <<= Type * (Expr >>= ((wf_expr - App) | Global | CreateClosure | ClosureCall | FunCall)))
          | (CreateClosure <<= (Fun >>= Ident) * Env * FreeVarList)
          | (FunCall <<= (Lhs >>= Expr) * (Rhs >>= Expr))
          | (ClosureCall <<= (Lhs >>= Expr) * (Rhs >>= Expr))
        | (Type <<= (Type >>= wf_types | ForAllTy | TPtr))
        ;
    }

  namespace LLVMIRCompilation{

    // FIXME: Only for reference.
    //        Manually unfolded WF from typechecker
    inline const auto wf_fresh =
    (Top <<= File)
    | (File <<= Program)
    | (Program <<= TopExpr++)
    | (TopExpr <<= (Let | Expr))
    | (Let <<= Ident * Type * Expr)
    | (Expr <<= Type * (Expr >>= wf_expr))
    | (If <<= Expr * Expr * Expr)
    | (Fun <<= FunDef)
    | (FunDef <<= Ident * Type * Param * Expr)
    | (Param <<= Ident * Type)
    | (App <<= (Lhs >>= Expr) * (Rhs >>= Expr))
    | (Mul <<= (Lhs >>= Expr) * (Rhs >>= Expr))
    | (Add <<= (Lhs >>= Expr) * (Rhs >>= Expr))
    | (Sub <<= (Lhs >>= Expr) * (Rhs >>= Expr))
    | (LT <<= (Lhs >>= Expr) * (Rhs >>= Expr))
    | (Equals <<= (Lhs >>= Expr) * (Rhs >>= Expr))
    | (Annotation <<= Type >>= (wf_types | TNone))
    | (TypeArrow <<= (Ty1 >>= wf_types) * (Ty2 >>= wf_types))
    | (Type <<= (Type >>= wf_types | ForAllTy))
    | (ForAllTy <<= TVars * Type)
    | (TVars <<= TVar++)
    ;

    

    // eq: equal
    // ne: not equal
    // ugt: unsigned greater than
    // uge: unsigned greater or equal
    // ult: unsigned less than
    // ule: unsigned less or equal
    // sgt: signed greater than
    // sge: signed greater or equal
    // slt: signed less than
    // sle: signed less or equal
    // Unsigned
    inline const auto wf_comparison = (EQ | NE | UGT | UGE | ULT | ULE | SGT | SGE | SLT | SLE);
    inline const auto wf_llvm_types = (Ti1 | Ti32 | Ti64 | TPtr | TypeArrow); 

    inline const auto wf_operand = (Int | Ident);
    
    inline const auto wf =
    (Top <<= Ident * Program)
    | (Program <<= (Action | IRFun)++[1])
    | (IRFun <<= TypeArrow * ParamList * Body)
      | (TypeArrow <<= (Ty1 >>= wf_llvm_types) * (Ty2 >>= wf_llvm_types))
      | (ParamList <<= Param++)
        | (Param <<= Ident * (Type >>= wf_llvm_types))
      | (Body <<= (Instr | Label | Action)++[1])
    // Builder actions, which aren't LLVM IR instructions.
    | (Action <<= (CreateConst | CreateStructType | CreateFunType | GetFunction | GetType))
      | (CreateConst <<= Ident * (Type >>= Ti64 | Ti32 | Ti1) * IRValue)
      | (CreateStructType <<= Ident * IRTypeList)
        | (IRTypeList <<= wf_llvm_types++)
      | (CreateFunType <<= (Result >>= Ident) * (IRType >>= wf_llvm_types) * IRTypeList)
        | (IRTypeList <<= wf_llvm_types++)
      | (GetFunction <<= (Result >>= Ident) * (Fun >>= Ident))
      | (GetType <<= (Result >>= Ident) * (IRType >>= Ident))
    // LLVM IR instructions.
    | (Instr <<= (BinaryOp | MemoryOp | TerminatorOp | MiscOp | ConversionOp))
    | (BinaryOp <<= (Add | Sub | Mul))
    // TODO: Refactor so operands should be Ident or IRValue,
    //       never `Int` in order to decouple this IR from MiniML.
      | (Add <<= (Result >>= Ident) * (Type >>= Ti32) * (Lhs >>= wf_operand) * (Rhs >>= wf_operand))
      | (Sub <<= (Result >>= Ident) * (Type >>= Ti32) * (Lhs >>= wf_operand) * (Rhs >>= wf_operand))
      | (Mul <<= (Result >>= Ident) * (Type >>= Ti32) * (Lhs >>= wf_operand) * (Rhs >>= wf_operand))
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
      | (Icmp <<= (Result >>= Ident) * (Op >>= wf_comparison) * (Type >>= (Ti1 | Ti32)) * (Lhs >>= wf_operand) * (Rhs >>= wf_operand))
      | (Phi <<= (Result >>= Ident) * (Type >>= wf_llvm_types) * PredecessorList)
        | (PredecessorList <<= Predecessor++)
          | (Predecessor <<= (IRValue >>= Ident) * Label)
    | (TerminatorOp <<= (Branch | Jump | Ret))
      | (Branch <<= (Cond >>= Ident) * (True >>= Label) * (False >>= Label))
      | (Jump <<= (Label))
      | (Ret <<= Ident)
    | (ConversionOp <<= (BitCast))
      | (BitCast <<= (Result >>= Ident) * (IRValue >>= Ident) * (IRType >>= (Ident | wf_llvm_types)))
    ;
  }

  namespace LLVMIRBlockify {

    inline const auto wf = LLVMIRCompilation::wf
    | (Body <<= (Block)++[1])
    | (Block <<= (Instr | Label | FunDef | Action)++[1])
    ;
  }

  namespace LLVMIRGeneration {

  inline const auto wf_operand = (Int | Ident);

  inline const auto wf =
    LLVMIRBlockify::wf - Action
    ;
      
    }
}
// clang-format on
