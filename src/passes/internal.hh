#pragma once
#include "../llvm-lang.hh"
#include "../miniml-lang.hh"

// clang-format off
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
        | (FunDef <<= Ident * Type * Param * FreeVarList * Expr)[Ident]
          | (FreeVarList <<= FreeVar++)
            | (FreeVar <<= Ident * Type)
        | (Expr <<= Type * (Expr >>= (wf_expr | Global)))
        ;

      inline const auto wf_functions =
        wf_freeVars
        | (Top <<= IRProgram)
        | (IRProgram <<= IRFun++)
        | (IRFun <<= Type * ParamList * Env * FreeVarList * Body)
        | (ParamList <<= Param++)
        | (Body <<= TopExpr++)
        ;

      inline const auto wf_closures =
        wf_functions
        | (IRProgram <<= (Env | IRFun)++[1])
        | (Env <<= Type++)
        | (Body <<= (TopExpr | Expr)++)
        | (TopExpr <<= (Let | Expr))
        | (Expr <<= Type * (Expr >>= ((wf_expr - App) | Global | CreateClosure | ClosureCall | FunCall)))
          | (CreateClosure <<= (Fun >>= Ident) * Env * FreeVarList)
          | (FunCall <<= (Fun >>= Expr) * (Param >>= Expr))
          | (ClosureCall <<= (Fun >>= Expr) * (Param >>= Expr))
        | (Type <<= (Type >>= wf_types | ForAllTy | TPtr))
        ;

      inline const auto wf =
        wf_closures
        | (FreeVar <<= (Ident >>= (Ident | Global)) * Type)
        ;
    }

    namespace LLVMIRCompilation{

      inline const auto wf = 
      llvmir::wf - llvmir::Block
      | (llvmir::Body <<= (llvmir::Label | llvmir::Action | llvmir::Instr | llvmir::TerminatorOp)++[1])
      ;
    }
}
// clang-format on
