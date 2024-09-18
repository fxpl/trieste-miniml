#include "../../miniml-lang.hh"
#include "../utils.hh"
#include "../internal.hh"


namespace miniml {

using namespace trieste;


  inline const auto expr_cmpOp = T(LT,Equals);
  PassDef comparison()
  {
    return {
        "comparison",
        parse::wf_cmp,
        dir::topdown, 
        {
          In(Expr) * (T(Expr)[Lhs] * expr_cmpOp[Op] * T(Expr)[Rhs]) >>
            [](Match &_)
              {return Expr << (_(Op) << _[Lhs] << _[Rhs]);
          },
          // Error rules
        expr_cmpOp[Op] << --(T(Expr) * T(Expr)) >>
          [](Match &_)
          {
            return err(_[Op], "invalid expression");
          },
        }};
  }
}