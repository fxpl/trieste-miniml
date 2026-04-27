#include "../../miniml-lang.hh"
#include "../internal.hh"
#include "../utils.hh"

namespace miniml {

  using namespace trieste;

  PassDef let() {
    return {
      "let",
      parse::wf_let,
      (dir::topdown),
      {T(Group)
           << (T(Let) 
             * T(Ident)[Ident] 
             * T(Equals) 
             * (T(Expr)[Expr] / (Any * Any++)[Rhs])) >>
         [](Match& _) {
           return TopExpr
                  << (Let << _(Ident) 
                  << (_(Expr) ? _(Expr) : Expr << _[Rhs]));
         },
       In(Program) * T(Group)[Group] << !T(Let) >>
         [](Match& _) { return TopExpr << (Expr << *_[Group]); },
       // error
       T(Let)[Let] << End >>
         [](Match& _) { return err(_(Let), "invalid let declaration"); },
       T(Group)[Group] >>
         [](Match& _) { return err(_(Group), "unresolved expression"); }}};
  }
}