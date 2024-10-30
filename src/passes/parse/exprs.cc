#include "../../miniml-lang.hh"
#include "../utils.hh"
#include "../internal.hh"

namespace miniml{
    
PassDef wrap_exprs(){
    return {
      "exprs",
      parse::wf_conditionals,
      (dir::bottomup | dir::once),
      {
      // wrap subexpressions as in Expr nodes 
      In(Expr) * ((T(Ident,Int,True,False,Fun,If)[Expr] * --End)
              / (--Start * T(Ident,Int,True,False,Fun,If)[Expr])) >> 
          [](Match& _){
            return Expr << _(Expr);
      },
      }
    };
  }
}