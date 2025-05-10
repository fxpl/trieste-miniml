#include "../../miniml-lang.hh"
#include "../internal.hh"
#include "../llvm_utils.hh"
#include "../utils.hh"
#include "trieste/token.h"

namespace miniml {

  using namespace trieste;

  struct Context {
    // <FunDef> -> <Ident>++
    std::map<Node, std::set<Node>> freeVars;
  };

  /**
   * Adds a Node containing all free (unbound) variables in a function body to
   * its definition.
   *
   * This pass must be bottom up to ensure Top is the last visited node.
   */
  PassDef free_variables() {
    auto context = std::make_shared<Context>();

    return {
      "free_variables",
      closures::wf_freeVars,
      (dir::bottomup | dir::once),
      {
        // Find free variables and their function definition.
        In(Expr) * T(Ident)[Ident] >> [context](Match& _) -> Node {
          // FIXME: debug print
          std::cout << "Ident: " << node_val(_(Ident)) << std::endl;

          Node ancestor = findClosestAncestor(_(Ident), FunDef);
          if (ancestor != nullptr) {
            // Ensures FunDef nodes without free variables are in the map.
            if (!context->freeVars.contains(ancestor)) {
              context->freeVars[ancestor];
            }

            auto defs = _(Ident)->lookup();
            if (!defs.empty()) {
              auto def = defs.front();

              if (def->type() == Let) {
                context->freeVars[ancestor].insert(_(Ident));
              }
            }
          }
          return _(Ident);
        },

        // Add a list of free variables to the function definition.
        T(Program)[Program] >> [context](Match& _) -> Node {
          for (auto pair : context->freeVars) {
            Node funDef = pair.first;

            Node freeVarList = FreeVarList;
            for (auto ident : pair.second) {
              Node expr = ident->parent();
              Node freeVar = FreeVar;
              freeVar->push_back(expr->back()->clone()); // Ident.
              freeVar->push_back(expr->front()->clone()); // Type.
              freeVarList->push_back(freeVar);
            }

            funDef->push_back(freeVarList);
          }
          return _(Program);
        },

      }};
  }
}
