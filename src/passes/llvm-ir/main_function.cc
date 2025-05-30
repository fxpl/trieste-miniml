#include "../../miniml-lang.hh"
#include "../internal.hh"
#include "trieste/token.h"

namespace miniml {

  using namespace trieste;

  /**
   * Inserts program into body of function "main",
   * required by LLVM
   */
  PassDef main_function() {
    return {
      "main_function",
      closures::wf_functions,
      dir::topdown | dir::once,
      {
        In(Top) * T(Program)[Program] >> [](Match& _) -> Node {
          return Reapply << (IRProgram << *_(Program));
        },

        In(Top) * (T(IRProgram)[Program] << (Any++)[TopExpr]) >>
          [](Match& _) -> Node {
          Node main = IRFun ^ "main";

          auto children = *_(Program);
          _(Program)->erase(_(Program)->begin(), _(Program)->end());

          // clang-format off
          return _(Program)
            << (main // TODO: Fix realistic typing: main(void) -> i32
                     << (Type << (TypeArrow << TInt << TInt))
                     << ParamList
                     << Env
                     << FreeVarList
                     << (Body << children));
          // clang-format on
        },

      }};
  }
}
