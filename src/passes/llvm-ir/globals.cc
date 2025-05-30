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
          Node ident = _(Ident);
          std::string identName = node_val(ident);
          auto defs = ident->lookup();
          if (!defs.empty()) {
            auto def = defs.front();

            if (def->type() == Let) {
              return Global ^ identName;
            }

            // Parameters from other functions are loaded from environment.
            Node parent = _(Ident)->parent(FunDef);
            if ((def->type() == Param || def->type() == FunDef) && def->parent(Fun)->front() != parent) {
              return Global ^ identName;
            }
          }
          return _(Ident);
        },

      }};
  }
}
