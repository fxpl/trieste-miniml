#pragma once
#include <trieste/trieste.h>

namespace miniml {
  using namespace trieste;

  // Program
  inline const auto Program =
    TokenDef("program", flag::symtab | flag::defbeforeuse);
  // Declarations
  inline const auto Let = TokenDef("let", flag::lookup);

  // --- Types ---
  inline const auto Type = TokenDef("type");
  inline const auto TNone = TokenDef("none");
  inline const auto TInt = TokenDef("t_int");
  inline const auto TBool = TokenDef("t_bool");
  inline const auto TVar = TokenDef("t_var", flag::print);
  inline const auto TypeArrow = TokenDef("->");
  // Constructed types
  inline const auto TVars = TokenDef("t_vars");
  inline const auto ForAllTy = TokenDef("forall");
  // Constraints
  inline const auto EqConstr = TokenDef("eq_constraint");
  inline const auto InstConstr = TokenDef("inst_constraint");
  inline const auto GenConstr = TokenDef("gen_constraint");
  inline const auto SubstEqConstr = TokenDef("subst_eq_constraint");
  inline const auto SubstInstConstr = TokenDef("subst_inst_constraint");
  inline const auto SubstGenConstr = TokenDef("subst_gen_constraint");
  inline const auto Constraints = TokenDef("constraints");

  // --- Expressions ---

  // Constants
  inline const auto Int = TokenDef("int", flag::print);
  inline const auto True = TokenDef("true");
  inline const auto False = TokenDef("false");

  // Arithmetic operators

  inline const auto Add = TokenDef("+");
  inline const auto Sub = TokenDef("-");
  inline const auto Mul = TokenDef("*");

  // Comparison expressions
  inline const auto LT = TokenDef("<");
  inline const auto Equals = TokenDef("=");

  // Functions
  inline const auto Fun = TokenDef(
    "fun", flag::symtab); // binds its argument to its symtab
  inline const auto FunDef = TokenDef("fundef", flag::lookup | flag::shadowing);
  inline const auto Param = TokenDef("param", flag::lookup | flag::shadowing);
  inline const auto Annotation = TokenDef("t_annotation");
  inline const auto Is = TokenDef("is");

  // Conditionals
  inline const auto If = TokenDef("if");
  inline const auto Then = TokenDef("then");
  inline const auto Else = TokenDef("else");
  inline const auto Cond = TokenDef("cond");

  // Identifiers
  inline const auto Ident = TokenDef("ident", flag::print);

  // Printing
  inline const auto Print = TokenDef("print");

  // Grouping tokens
  inline const auto TopExpr =
    TokenDef("topexpr"); // expressions/let decl at top level
  inline const auto Expr = TokenDef("expr");
  inline const auto App = TokenDef("app"); // function application

  // Separators
  inline const auto Colon = TokenDef(":");
  inline const auto Term = TokenDef(";;");
  inline const auto Paren = TokenDef("()");

  // Convenience tokens
  inline const auto Name = TokenDef("name", flag::print);
  inline const auto Op = TokenDef("op");
  inline const auto Lhs = TokenDef("lhs");
  inline const auto Rhs = TokenDef("rhs");
  inline const auto Ty1 = TokenDef("ty1");
  inline const auto Ty11 = TokenDef("ty11");
  inline const auto Ty12 = TokenDef("ty12");
  inline const auto Ty2 = TokenDef("ty2");
  inline const auto Ty21 = TokenDef("ty21");
  inline const auto Ty22 = TokenDef("ty22");
  inline const auto Constr = TokenDef("constraint");
  inline const auto Result = TokenDef("result");

  /**
   * Compilation to LLVM IR
   */
  inline const auto Compile = TokenDef("compile");
  inline const auto PropagateCompile = TokenDef("propagate_compile");
 
  // Closure implementation
  inline const auto IRProgram = TokenDef("ir_program");
  inline const auto IRFun = TokenDef("ir_function", flag::print | flag::symtab);
  inline const auto Env = TokenDef("env", flag::print);
  inline const auto ParamList = TokenDef("param_list");
  inline const auto CreateClosure = TokenDef("create_closure", flag::print);
  inline const auto ClosureCall = TokenDef("closure_call");
  inline const auto FunCall = TokenDef("fun_call");
  inline const auto Body = TokenDef("body");
  inline const auto FreeVarList = TokenDef("free_var_list");
  inline const auto FreeVar = TokenDef("free_variable");
  inline const auto TPtr = TokenDef("pointer");
  
  inline const auto Global = TokenDef("global", flag::print);
}
