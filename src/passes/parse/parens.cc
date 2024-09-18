#include "../../miniml-lang.hh"
#include "../internal.hh"
#include "../utils.hh"


namespace miniml {

using namespace trieste;

  inline const auto exprs = expr_binOp / expr_const / expr_keywords / T(Fun,Ident,Expr); 
  
  PassDef parens(){
      return {
      "parens",
      parse::wf_parens,
      dir::topdown,
      {
      T(Paren) << T(Group)[Group] >> 
      [](Match& _) { 
          return Expr << *_[Group];
      },
      T(Paren)[Paren] >>
        [](Match& _){
          return err(_(Paren), "invalid parenthesis");
        },
      In(Expr)[Expr] * !(exprs) >>
        [](Match& _){
          return err(_(Expr), "invalid expression");
      }
      }
    };
  }
}