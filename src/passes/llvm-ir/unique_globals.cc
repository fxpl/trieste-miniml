#include "../../miniml-lang.hh"
#include "../internal.hh"
#include "../utils.hh"
#include "trieste/token.h"

namespace miniml {

  using namespace trieste;

  struct UniqueGlobalsContext {
    // Names bound locally anywhere in the program (parameters and function
    // names).
    std::set<std::string> localNames;
  };

  inline void collect_local_names(Node node, std::set<std::string>& names) {
    if (node->type().in({Param, FunDef}) && !node->empty() &&
        node->front() == Ident) {
      names.insert(node_val(node->front()));
    }
    for (auto& child : *node) {
      collect_local_names(child, names);
    }
  }

  /**
   * Backend name of a top-level `let` definition.
   *
   * The backend refers to top-level definitions by name only (`Global`,
   * free-variable lists, the declared types of globals in compile), and a
   * captured parameter or function name also becomes a `Global` with its own
   * name. So a top-level definition is renamed when its name would be
   * ambiguous there:
   *  - every definition after the first one of a name gets `name$i`
   *    (`let x = 5;; let x = x + 1;;`; the type checker already resolves each
   *    use to the latest definition before it, as Program is `defbeforeuse`),
   *  - the first one gets `name$0` if the name is also bound locally somewhere
   *    (`let g = ...;; let f = fun h (g : int) is ...`).
   * `$` cannot occur in source identifiers, so these never clash.
   */
  inline std::string unique_global_name(
    Node let, const std::set<std::string>& localNames) {
    std::string name = node_val(let / Ident);
    Node program = let->parent(Program);
    if (!program) {
      return name;
    }
    // The symbol table is only rebuilt after this pass, so this still lists
    // every definition of the original name, in program order.
    Nodes defs = program->look((let / Ident)->location());
    for (size_t i = 0; i < defs.size(); i++) {
      if (defs[i] == let) {
        if (i == 0 && !localNames.contains(name)) {
          return name;
        }
        return name + "$" + std::to_string(i);
      }
    }
    return name;
  }

  /**
   * Gives every top-level definition a name that the by-name references of
   * the backend passes resolve unambiguously.
   */
  PassDef unique_globals() {
    auto ctx = std::make_shared<UniqueGlobalsContext>();

    PassDef pass = {
      "unique_globals",
      check::wf,
      (dir::bottomup | dir::once),
      {
        // A use of a top-level definition: the latest definition before it
        In(Expr) * T(Ident)[Ident] >> [ctx](Match& _) -> Node {
          auto defs = _(Ident)->lookup();
          if (defs.empty() || defs.back()->type() != Let) {
            return NoChange;
          }
          std::string name = unique_global_name(defs.back(), ctx->localNames);
          if (name == node_val(_(Ident))) {
            return NoChange;
          }
          return Ident ^ name;
        },

        // The defining occurrence
        In(Let) * T(Ident)[Ident] >> [ctx](Match& _) -> Node {
          std::string name =
            unique_global_name(_(Ident)->parent(), ctx->localNames);
          if (name == node_val(_(Ident))) {
            return NoChange;
          }
          return Ident ^ name;
        },
      }};

    pass.pre([ctx](Node root) {
      ctx->localNames.clear();
      collect_local_names(root, ctx->localNames);
      return 0;
    });

    return pass;
  }
}
