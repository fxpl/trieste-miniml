#pragma once
#include "../miniml-lang.hh"

namespace miniml{
 using namespace trieste;  

    std::vector<Pass> passes();

    inline const auto wf_types = TInt | TBool | TVar | TypeArrow ; 
    inline const auto wf_expr = Int | True | False 
    | Add | Sub | Mul | LT | Equals
    | If | Ident | Fun | App ;

    namespace init_parse{

    Parse parser();

    inline const auto wf_parse_tokens = 
     Add | Sub | Mul | 
     Equals | LT |
     Int | True | False | 
     If | Then | Else |
     Ident | Fun | Is | Let | 
     Colon | Paren | Term | 
     wf_types
     ;
    
    inline const wf::Wellformed wf =
      (Top <<= File)
    | (File <<= (Term | Group)) 
    | (Paren <<= Group)
    | (Term <<= (Group)++[1])
    | (Group <<= (wf_parse_tokens)++[1])
    ;

    }
    namespace parse{

    inline const auto wf_parse_cleanup = init_parse::wf 
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
  
    inline const auto wf_parens =
    wf_fun 
    | (Expr <<= (wf_exp_tokens_par)++[1])
    | (Group <<= ((wf_group_tokens | Let) - Paren)++[1])
    ;
    inline const auto wf_let =
      wf_parens
    | (Program <<= TopExpr++) 
    | (TopExpr <<= (Let | Expr))
    | (Let <<= Ident * Expr)
    | (Expr <<= (wf_exp_tokens_par)++[1])
    ;
    inline const auto wf_exp_tokens_cond = wf_exp_tokens_par - Then - Else; 

    inline const auto wf_conditionals =
      wf_let
    | (If <<= Expr * Expr * Expr)
    | (Expr <<= (wf_exp_tokens_cond)++[1])
    ;

    inline const auto wf_exp_tokens_app =
      wf_exp_tokens_cond | App; 

    inline const auto wf_funapp =
      wf_conditionals
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
    | (Constraints <<= (EqConstr | InstConstr | GenConstr)++) 
    | (Param <<= Ident * Type)[Ident] // bind fun arguments to symtab
    | (Let <<= Ident * Type * Expr)[Ident] // bind let variable to symtab
    | (EqConstr <<= (Ty1 >>= wf_types) * (Ty2 >>= wf_types)) 
    | (InstConstr <<= (Ty1 >>= wf_types) * (Ty2 >>= TVar)) // Ty1 is an instance of Ty2
    | (GenConstr <<= (Ty1 >>= wf_types) * (Ty2 >>= wf_types)) //Ty is a generalization of Ty2
    | (Type <<= (Type >>= wf_types)) 
    | (FunDef <<= Ident * Type * Param * Expr)[Ident] // to access nested functions
    | (Type <<= (Type >>= wf_types))
    ;

    inline const auto wf_inf_exprs = wf_fresh 
    | (Expr <<= Type * (Expr >>= wf_expr));

    inline const auto wf_solve_constr =
    wf_inf_exprs 
    | (ForAllTy <<= TVars * Type) 
    | (TVars <<= TVar++) 
    | (Program <<= (Let | Expr)++)
    | (Type <<= (Type >>= wf_types | ForAllTy)) 
    | (Let <<= Ident * Type * Expr) // remove binding to reduce clutter
    | (Param <<= Ident * Type) // remove binding to reduce clutter                                                                                                                                                                                                                                                                                                      // remove binding to reduce clutter
    | (FunDef <<= Ident * Type * Param * Expr) // remove binding to reduce clutter                                                                                                                                                                                                                                                                                     // remove binding to reduce clutter
    ;

    // Final Wf
    inline const auto wf = 
    (Program <<= (Let | Expr)++)
    | (Expr <<= Type * (Expr >>= wf_expr))
    | (Let <<= Ident * Type * Expr) //remove binding to reduce clutter
    | (Param <<= Ident * Type) //remove binding to reduce clutter
    | (FunDef <<= Ident * Type * Param * Expr) //remove binding to reduce clutter
    | (Type <<= (Type >>= wf_types | ForAllTy))
    | (ForAllTy <<= TVars * Type)
    | (TVars <<= TVar++);

    }

}
  
   