// TODO: Try to not include all the passes here
// clang-format off
#include "../init_parse.cc"
#include "parse/parse_cleanup.cc"
#include "parse/fun.cc"
#include "parse/parens.cc"
#include "parse/let.cc"
#include "parse/exprs.cc"
#include "parse/conditionals.cc"
#include "parse/app.cc"
#include "parse/arith.cc"
#include "parse/comparison.cc"
#include "parse/cleanup.cc"

#include "typecheck/inference.cc"
#include "typecheck/solve_constraints.cc"
#include "typecheck/resolve_print.cc"

#include "llvm-ir/compile.cc"
#include "llvm-ir/llvmIR.cc"
// clang-format on

namespace miniml {

  Parse parser() {
    return init_parse::parser();
  }

  std::vector<Pass> passes() {
    return {
      // Parsing
      parse_cleanup(),
      fun(),
      parens(),
      conditionals(),
      let(),
      wrap_exprs(),
      funapp(),
      mul(),
      addsub(),
      comparison(),
      cleanup(),

      // Typechecking
      inf_fresh(),
      inf_exprs(),
      let_constr(),
      solve_constraints(),
      cleanup_constraints(),
      resolve_print(),

      // LLVM IR generation
      compile(),
      generateLLVMIR(),
    };
  }
}
