#include "../../miniml-lang.hh"
#include "../utils.hh"
#include "../internal.hh"

namespace miniml {

using namespace trieste;
PassDef cleanup() {
    return {
        "cleanup",
        parse::wf,
        (dir::bottomup | dir::once), {
          //remove redundant exprs 
        T(Expr) << T(Expr)[Expr] >>
            [](Match &_)
              {return _(Expr);
          },
        // error 
        T(Expr)[Expr] << (Any * Any * Any++) >>
            [](Match &_)
              {return err(_(Expr),"invalid expression");
          },
        }
      };
  }
}  