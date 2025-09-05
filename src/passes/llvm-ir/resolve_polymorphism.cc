#include "../internal.hh"
#include "../utils.hh"

namespace miniml {
  using namespace trieste;

  // Ensure that the program does not contain polymorphic types.
  PassDef resolve_polymorphism() {
    return {
      "resolve_polymorphism",
      check::wf,
      (dir::bottomup),
      {
        T(TVar)[TVar] >>
          [](Match& _) {
            return err(_(TVar), "Cannot compile polymorphic types");
          },
      }};
  }
}
