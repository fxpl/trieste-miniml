#include "../../miniml-lang.hh"
#include "../internal.hh"
#include "../utils.hh"
#include "trieste/token.h"

namespace miniml {

  using namespace trieste;

  struct Context {
    // <FunDef> -> <Ident>++
    std::map<Node, std::set<Node>> freeVars;
    // To avoid duplicate free variables.
    std::map<Node, std::set<std::string>> freeVarNames;
  };

  /**
   * This pass must be bottom up to ensure Top is the last visited node.
   */

  PassDef free_variables() {
    auto ctx = std::make_shared<Context>();

    return {
      "free_variables",
      closures::wf_freeVars,
      (dir::bottomup | dir::once),
      {
        // Find free variables and their function definition.
        In(Expr) * T(Ident)[Ident] >> [ctx](Match& _) -> Node {
          Node ident = _(Ident);
          std::string name = node_val(ident);

          Node enclosingFun = ident->parent(FunDef);
          if (enclosingFun == nullptr) {
            return ident;
          }

          auto defs = ident->lookup();
          if (defs.empty()) {
            return ident;
          }

          std::set<std::string>* capturedNames =
            &ctx->freeVarNames[enclosingFun];
          if (capturedNames->contains(name)) {
            return ident;
          }

          Node def = defs.front();
          if (
            (def->type() == Let) ||
            (def->type() == Param &&
             def->parent(Fun)->front() != enclosingFun)) {
            ctx->freeVars[enclosingFun].insert(ident);
            capturedNames->insert(name);
          }

          return NoChange;
        },

        // Ensure all FunDefs are appended with a FreeVarList
        T(FunDef)[FunDef] >> [ctx](Match& _) -> Node {
          Node funDef = _(FunDef);

          if (!ctx->freeVars.contains(funDef)) {
            ctx->freeVars[funDef];
            ctx->freeVarNames[funDef];
          }

          return NoChange;
        },

        // Add a list of free variables to the function definition.
        T(Program)[Program] >> [ctx](Match& _) -> Node {
          for (auto pair : ctx->freeVars) {
            Node funDef = pair.first;
            auto freeIdents = pair.second;
            Node freeVarList = FreeVarList;

            for (auto ident : freeIdents) {
              Node type = ident->parent() / Type;
              Node freeVar = FreeVar;

              freeVar->push_back(ident->clone());
              freeVar->push_back(type->clone());

              freeVarList->push_back(freeVar);
            }

            // Keep Body as Fun's last child for readability.
            Node body = funDef->pop_back();
            funDef->push_back(freeVarList);
            funDef->push_back(body);
          }

          return _(Program);
        },
      }};
  }
}
