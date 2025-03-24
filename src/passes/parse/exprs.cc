#include "../../miniml-lang.hh"
#include "../utils.hh"
#include "../internal.hh"

namespace miniml{

PassDef wrap_exprs(){
    return {
      "exprs",
      parse::wf_let,
      dir::bottomup,
      {
      In(Expr) * ((T(Ident,Int,True,False,Fun,If)[Ident] * --End)
              / (--Start * T(Ident,Int,True,False,Fun,If)[Ident])) >>
          [](Match& _){
            return Expr << _(Ident);
      },
      --(In(FunDef,Param,Let,Expr)) * T(Ident)[Ident] >> //identifier expressions
        [](Match& _){
          return Expr << _(Ident);
      },
      --(In(Expr)) * ((T(Int,True,False,Fun,If)[Expr] * --End)
                   / (--Start * T(Int,True,False,Fun,If)[Expr])) >>
        [](Match& _){
          return Expr << _(Expr);
        }
      }
    };
  }
}
