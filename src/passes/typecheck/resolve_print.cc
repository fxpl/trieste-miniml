#include "../internal.hh"
#include "../utils.hh"

//Infer types and constraints

namespace miniml
{

  using namespace trieste;
  using namespace wf::ops;

  // Ensure that we do not print polymorphic values or use print as a first-class value
  PassDef resolve_print()
  {
    return {
        "resolve_print",
        check::wf,
        (dir::bottomup),
        {
            In(App) *
            T(Expr) << ((T(Type) << (T(TypeArrow) << T(TVar))) * T(Print)) >>
                [](Match &_)
                {
                  return err(_(Print), "Cannot print polymorphic values");
                },
            --In(App) * (T(Expr) << (T(Type) * T(Print)[Print])) >>
                [](Match &_)
                {
                  return err(_(Print), "Cannot use print as a first-class value");
                },

            In(App) * T(Expr) * (T(Expr) << (T(Type) * T(Print)[Print])) >>
                [](Match &_)
                {
                  return err(_(Print), "Cannot use print as a first-class value");
                },
        }};
  }
}
