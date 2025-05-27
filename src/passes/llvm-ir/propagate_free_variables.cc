#include "../../miniml-lang.hh"
#include "../internal.hh"
#include "../utils.hh"
#include "trieste/token.h"

namespace miniml {

  using namespace trieste;

  /**
   * Propagates free variables in a lambda up to the enclosing scopes,
   * until reaching the scope where the free variable is defined.
   */
  PassDef propagate_free_variables() {
    return {
      "propagate_free_variables",
      closures::wf_freeVars,
      (dir::bottomup | dir::once),
      {
        // Find free variables and their function definition.
        In(FreeVarList) * T(FreeVar)[FreeVar] >> [](Match& _) -> Node {
          Node freeVar = _(FreeVar);
          Node ident = freeVar / Ident;
          std::string name = node_val(ident);

          Node enclosingScope = ident->scope()->scope();
          if (enclosingScope != Fun) {
            return freeVar;
          }

          if (ident->lookup(enclosingScope).empty()) {
            Node enclosingFreeVars = enclosingScope / FunDef / FreeVarList;
            for (Node enclosedFreeVar : *enclosingFreeVars) {
              std::string enclosedName = node_val(enclosedFreeVar / Ident);
              if (name == enclosedName) {
                return freeVar;
              }
            }

            enclosingFreeVars->push_back(freeVar->clone());
          }

          return freeVar;
        },
      }};
  }
}
