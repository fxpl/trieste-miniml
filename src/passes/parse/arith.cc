#include "../../miniml-lang.hh"
#include "../utils.hh"
#include "../internal.hh"

namespace miniml {

using namespace trieste;


  inline const detail::Pattern operand = T(Int,Ident,Expr);
  
  PassDef mul()
  {
    return {
        "mul",
        parse::wf_mul,
        dir::topdown,
        {
          In(Expr) * (T(Expr)[Lhs] * T(Mul)[Op] * T(Expr)[Rhs]) >>
            [](Match &_)
              {return Expr << (_(Op) << _[Lhs] << _[Rhs]);
          }, 
          // Error rule
          T(Mul)[Mul] << --(T(Expr) * T(Expr)) >>
            [](Match &_)
            {
              return err(_(Mul), "invalid expression");
            },
        }};
  }
  
  PassDef addsub()
  {
    return {
        "addsub",
        parse::wf_add,
        dir::topdown,
        {
          In(Expr) * (T(Expr)[Lhs] * T(Add,Sub)[Op] * T(Expr)[Rhs]) >>
            [](Match &_)
              {return Expr << (_(Op) << _[Lhs] << _[Rhs]);
            },
          // Error
          T(Add,Sub)[Add] << --(T(Expr) * T(Expr)) >>
            [](Match &_){
              return err(_[Add], "invalid add expression");
            },
        }};
  }
}