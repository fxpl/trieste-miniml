#include "parse/init_parse.cc"
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

namespace miniml{

Parse parser(){
  return init_parser();
}

std::vector<Pass> passes(){
    return {
      parse_cleanup(),
      fun(),
      parens(),
      let(),
      conditionals(),
      wrap_exprs(),
      funapp(),
      mul(),
      addsub(),
      comparison(),
      cleanup(),
      inf_fresh(),
      inf_exprs(),
      let_constr(),
      solve_constraints(),
      cleanup_constraints(),
      };
    }
}
