#include "../../miniml-lang.hh"
#include "../internal.hh"
#include "../utils.hh"
#include "trieste/token.h"

namespace miniml {

  using namespace trieste;

  /**
   * Replaces all `Ident` FreeVar tokens that refer to memory,
   * rather than a register, into `Global` for closure creation.
   */
  PassDef closure_globals() {
    return {
      "closure_globals",
      closures::wf,
      dir::bottomup | dir::once,
      {
        In(CreateClosure) * T(FreeVarList)[FreeVarList] >>
          [](Match& _) -> Node {
          Node freeVarList = _(FreeVarList);

          for (size_t i = 0; i < freeVarList->size(); i++) {
            Node freeVar = freeVarList->at(i);
            Node ident = freeVar / Ident;
            Node type = freeVar / Type;

            auto defs = ident->lookup();
            if (defs.empty() == true || defs.front()->type() != Param) {
              freeVarList->replace_at(
                i, FreeVar << (Global ^ node_val(ident)) << type);
            }
          }

          return freeVarList;
        },
      }};
  }
}
