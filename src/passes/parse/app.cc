#include "../../miniml-lang.hh"
#include "../utils.hh"
#include "../internal.hh"


namespace miniml {

using namespace trieste;
  
  PassDef funapp() {
    return {
        "funapp",
        parse::wf_funapp,
        (dir::bottomup) ,
        {
        // Fun application
        In(Expr) * T(Expr)[Lhs] * T(Expr)[Rhs] >>
            [](Match &_)
              {return Expr << (App
                        << _(Lhs)
                        << _(Rhs));
            },  
        // Error rules
        T(App)[App] << --(T(Expr) * T(Expr)) >>
          [](Match &_)
          {
            return err(_(App), "invalid expression");
          },
        }};
  }
}