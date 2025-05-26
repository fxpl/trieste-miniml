#include "../../miniml-lang.hh"
#include "../internal.hh"
#include "../utils.hh"
#include "trieste/token.h"

namespace miniml {

  using namespace trieste;

  /**
   * Converts inline lambdas into closures and function applications into
   * closure applications.
   *
   * For each inline lambda the pass performs the following actions:
   * - Creates an environment type declaration.
   * - Converts the function into a lambda (lifted to program level).
   * - Creates closure inplace of old fun declaration.
   */
  PassDef closure_conversion() {
    return {
      "closure_conversion",
      closures::wf,
      // bottom-up so nested lambdas get lifted first
      dir::bottomup,
      {
        In(Expr) *
            (T(Type) *
             (T(Fun)
              << (T(FunDef)
                  << (T(Ident)[Ident] * T(Type)[Type] * T(Param)[Param] *
                      T(Expr)[Expr] * T(FreeVarList)[FreeVarList])))) >>
          [](Match& _) -> Node {
          std::string uniqueId = std::string(_(Ident)->fresh().view());
          Node env = Env ^ "env_" + uniqueId;

          for (Node freeVar : *_(FreeVarList)) {
            Node type = freeVar->back();
            env->push_back(type->clone());
          }

          Node lambda = IRFun ^ "lambda_" + uniqueId;

          return Seq << (Lift << Program << env)
                     << (Lift
                         << Program
                         << (lambda << _(Ident) << _(Type)
                                    << (ParamList << (Param << (Ident ^ "env")
                                                            << (Type << TPtr))
                                                  << _(Param))
                                    << env->clone() << (Body << _(Expr))
                                    << _(FreeVarList)->clone()))
                     << (Type << TPtr)
                     << (CreateClosure << (Ident ^ node_val(lambda))
                                       << env->clone()
                                       << _(FreeVarList)->clone());
        },

        In(Expr) * (T(App)[App] << T(Expr)[Fun] * T(Expr)[Param]) >>
          [](Match& _) -> Node {
          // In miniML, everything but Print is a closure call.
          if (_(Fun) / Expr == Print) {
            return FunCall << *_(App);
          } else {
            return ClosureCall << *_(App);
          }
        },
      }};
  }
}
