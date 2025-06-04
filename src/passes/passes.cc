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

#include "llvm-ir/resolve_polymorphism.cc"

#include "llvm-ir/free_variables.cc"
#include "llvm-ir/propagate_free_variables.cc"
#include "llvm-ir/globals.cc"
#include "llvm-ir/main_function.cc"
#include "llvm-ir/closure_conversion.cc"
#include "llvm-ir/closure_globals.cc"

#include "llvm-ir/compile.cc"
#include "llvm-ir/blockify.cc"

#include "llvm-ir/code_generation.cc"
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

      // TODO: Polymorphism not supported
      resolve_polymorphism(),

      // Generate IR
      free_variables(),
      propagate_free_variables(),
      globals(),
      main_function(),
      closure_conversion(),
      closure_globals(),

      // Compile to LLVM IR WF
      compile(),
      blockify(),

      // Code generation
      llvmir::code_generation(),
    };
  }
}
