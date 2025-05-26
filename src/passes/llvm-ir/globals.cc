#include "../../miniml-lang.hh"
#include "../internal.hh"
#include "../utils.hh"
#include "trieste/token.h"

namespace miniml {

  using namespace trieste;

  /**
   * Replaces all `Ident` tokens that refer to memory,
   * rather than a register, into `Global`.
   */
  PassDef globals() {
    return {
      "globals",
      closures::wf_freeVars,
      (dir::bottomup | dir::once),
      {
        // Find free variables and their function definition.
        In(Expr) * T(Ident)[Ident] >> [](Match& _) -> Node {
          auto defs = _(Ident)->lookup();
          if (!defs.empty()) {
            auto def = defs.front();

            if (def->type() == Let) {
              return Global ^ node_val(_(Ident));
            }

            // Parameters from other functions are loaded from environment.
            Node parent = _(Ident)->parent(FunDef);
            if (def->type() == Param && def->parent(Fun)->front() != parent) {
              return Global ^ node_val(_(Ident));
            }
          }
          return _(Ident);
        },

      }};
  }
}
