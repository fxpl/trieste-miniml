#pragma once
#include <trieste/trieste.h>

namespace miniml
{
  using namespace trieste;

  // Program
  inline const auto Program = TokenDef("program", flag::symtab | flag::defbeforeuse);
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
  //Constraints
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
  inline const auto Fun  = TokenDef("fun", flag::symtab); //binds its argument to its symtab
  inline const auto FunDef = TokenDef("fundef", flag::lookup | flag::shadowing);
  inline const auto Param = TokenDef("param", flag::lookup | flag::shadowing);
  inline const auto Annotation = TokenDef("t_annotation");
  inline const auto Is  = TokenDef("is");

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
  inline const auto TopExpr = TokenDef("topexpr"); // expressions/let decl at top level
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

  /**
   * LLVM IR tokens
   */
  inline const auto Compile = TokenDef("compile");

  inline const auto Instr = TokenDef("instr");
  inline const auto BinaryOp = TokenDef("binary_op");
  inline const auto TerminatorOp = TokenDef("terminator_op");
  
  // MemoryOps
  inline const auto MemoryOp = TokenDef("memory_op");
  inline const auto Alloca = TokenDef("alloca");
  inline const auto Load = TokenDef("load");
  inline const auto Store = TokenDef("store");
  
  // MiscOps
  inline const auto MiscOp = TokenDef("misc_op");
  inline const auto Call = TokenDef("call");
  inline const auto Icmp = TokenDef("icmp");
  
  // Meta operations (Not apart of LLVM IR)
  inline const auto Meta = TokenDef("meta");
  inline const auto RegCpy = TokenDef("reg_cpy");
  inline const auto RegMap = TokenDef("reg_map");
  inline const auto FuncMap = TokenDef("func_map");
  
  // Helpers
  inline const auto Global = TokenDef("global");
  inline const auto IRValue = TokenDef("value");
  inline const auto Src = TokenDef("src");
  inline const auto Dst = TokenDef("dst");
  inline const auto Result = TokenDef("result");

  // LLVM IR Types
  inline const auto Ti32 = TokenDef("i32");
  inline const auto Ti1 = TokenDef("i1");

  // Comparison operations
  inline const auto Comp = TokenDef("comp");
  inline const auto Eq = TokenDef("eq");
  inline const auto Ne = TokenDef("ne");
  // Unsigned
  inline const auto Ugt = TokenDef("ugt");
  inline const auto Uge = TokenDef("uge");
  inline const auto Ult = TokenDef("ult");
  inline const auto Ule = TokenDef("ule");
  // Signed
  inline const auto Sgt = TokenDef("sgt");
  inline const auto Sge = TokenDef("sge");
  inline const auto Slt = TokenDef("slt");
  inline const auto Sle = TokenDef("sle");
}
