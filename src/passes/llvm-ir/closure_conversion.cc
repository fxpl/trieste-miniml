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
      closures::wf_closures,
      // bottom-up so nested lambdas get lifted first
      dir::bottomup,
      {
        In(Expr) * (T(Type) * T(Fun)[Fun]) >> [](Match& _) -> Node {
          Node fun = _(Fun);
          Node funDef = fun / FunDef;
          Node internalFunName = funDef / Ident;
          Node funType = funDef / Type;
          Node param = funDef / Param;
          Node freeVarList = funDef / FreeVarList;
          Node expr = funDef / Expr;

          std::string uniqueId = std::string(internalFunName->fresh().view());
          Node env = Env ^ "env_" + uniqueId;
          for (Node freeVar : *freeVarList) {
            Node FreeVarType = freeVar / Type;
            env->push_back(FreeVarType->clone());
          }

          Node lambda = IRFun ^ "lambda_" + uniqueId;
          Node lambdaId = Ident ^ node_val(lambda);
          Node envName = (Ident ^ "env");

          Node pointerType = Type << TPtr;

          // clang-format off
          return Seq << (Lift << IRProgram << env)
                     << (Lift << IRProgram
                         << (lambda
                             << funType
                             << (ParamList
                                 << (Param << internalFunName << pointerType)
                                 << (Param << envName << pointerType->clone())
                                 << (param))
                             << (env->clone())
                             << (freeVarList->clone())
                             << (Body << expr)))
                     << funType->clone()
                     << (CreateClosure << lambdaId
                                       << env->clone()
                                       << freeVarList->clone());
          // clang-format on
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
