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
          std::string name = node_val(_(Ident));

          Node enclosingScope = ident->parent(FunDef);
          if (enclosingScope == nullptr) {
            return ident;
          }

          auto defs = ident->lookup();
          if (defs.empty()) {
            return ident;
          }

          std::set<std::string>* capturedNames = &ctx->freeVarNames[enclosingScope];
          if (capturedNames->contains(name)) {
            return ident;
          }

          Node def = defs.front();
          if (
            (def->type() == Let) ||
            (def->type() == Param &&
             def->parent(Fun)->front() != enclosingScope)) {
            std::cout << "Found free variable: " << name
                      << " in function definition: " << node_val(enclosingScope)
                      << std::endl;

            ctx->freeVars[enclosingScope].insert(ident);
            capturedNames->insert(name);
          }

          return ident;
        },

        // Ensure all FunDefs are appended with a FreeVarList
        T(FunDef)[FunDef] >> [ctx](Match& _) -> Node {
          Node funDef = _(FunDef);

          if (!ctx->freeVars.contains(funDef)) {
            ctx->freeVars[funDef];
            ctx->freeVarNames[funDef];
          }

          return _(FunDef);
        },

        // Add a list of free variables to the function definition.
        T(Program)[Program] >> [ctx](Match& _) -> Node {
          for (auto pair : ctx->freeVars) {
            Node funDef = pair.first;
            Node freeVarList = FreeVarList;

            for (auto ident : pair.second) {
              Node expr = ident->parent();
              Node exprId = expr->back();
              Node exprTy = expr->front();
              Node freeVar = FreeVar;

              freeVar->push_back(exprId->clone());
              freeVar->push_back(exprTy->clone());

              freeVarList->push_back(freeVar);
            }

            Node body = funDef->pop_back();
            funDef->push_back(freeVarList);
            funDef->push_back(body);
          }

          return _(Program);
        },
      }};
  }
}
