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
          for (size_t i = 0; i < _(FreeVarList)->size(); i++) {
            Node freeVar = _(FreeVarList)->at(i);
            Node ident = freeVar / Ident;
            Node type = freeVar / Type;

            auto defs = ident->lookup();
            auto def = defs.front();

            if (def->type() != Param) {
              _(FreeVarList)
                ->replace_at(i, FreeVar << (Global ^ node_val(ident)) << type);
            }
          }

          return _(FreeVarList);
        },
      }};
  }
}
