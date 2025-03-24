#include "../../miniml-lang.hh"
#include "../utils.hh"
#include "../internal.hh"


namespace miniml {

using namespace trieste;

inline const auto empty_expr = T(Expr) << End;

  PassDef conditionals() {
    return {
      "conditionals",
      parse::wf_conditionals,
      dir::bottomup | dir::once,
      {
        (T(If) << (T(Group))[Cond]) * (T(Then) << (T(Group))[Then]) * (T(Else) << (T(Group))[Else]) >>
        [](Match &_) -> Node {
          return If << (Expr << *_[Cond])
                    << (Expr << *_[Then])
                    << (Expr << *_[Else]);
        },

        // Error rules
        T(If)[If] << End >>
        [](Match &_) -> Node {
          return err(_(If), "expected condition after 'if'");
        },

        T(Then)[Then] << End >>
        [](Match &_) -> Node {
          return err(_(Then), "expected expression after 'then'");
        },

        T(Else)[Else] << End >>
        [](Match &_) -> Node {
          return err(_(Else), "expected expression after 'else'");
        },

        T(If)[If] * ~(!T(Then)) >>
        [](Match &_) -> Node {
          return err(_(If), "expected 'then' after 'if'");
        },

        T(If) * T(Then)[Then] * ~(!T(Else)) >>
        [](Match &_) -> Node {
          return err(_(Then), "expected 'else' after 'then'");
        },

        T(Then)[Then] >>
        [](Match &_) -> Node {
          return err(_(Then), "unexpected 'then'");
        },

        T(Else)[Else] >>
        [](Match &_) -> Node {
          return err(_(Else), "unexpected 'else'");
        },

      }
    };
  }
}
