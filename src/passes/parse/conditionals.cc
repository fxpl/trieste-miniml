#include "../../miniml-lang.hh"
#include "../utils.hh"
#include "../internal.hh"


namespace miniml {

using namespace trieste;

inline const auto empty_expr = T(Expr) << End;
 
  PassDef conditionals() {
    return {
      "conditionals",
      parse::wf_conditionals,
      dir::topdown,
      {
        // nest conditional expressions 
        In(Expr,Group) * 
          (T(If) 
        * ((Any * --T(Then))++ * !T(Then))[Expr] 
        * T(Then) 
        * ((!T(Else) * --T(Else))++ * ~!T(Else))[Then] 
        * ~T(Else)
        * (Any++)[Else]) >>
        [](Match& _) { 
            return If << (Expr << _[Expr])
                      << (Expr << _[Then])
                      << (Expr << _[Else]);
        },
        //error 
        In(If)[If] * !T(Expr) >> 
        [](Match& _) { 
            return err(_(If), "invalid if");
        },
        T(If)[If] << ((T(Expr) * empty_expr * Any) / End) >> 
        [](Match& _) { 
            return err(_(If), "empty or missing then branch");
        },
        T(If)[If] << (T(Expr) * T(Expr) * empty_expr) >> 
        [](Match& _) { 
            return err(_(If), "empty or missing else branch");
        },
        --T(If) * T(Then,Else)[Expr] >> 
        [](Match& _) { 
          auto n = _(Expr)->str();
          return err(_(Expr), n+" only allowed inside if");
        },
      
      }
    };
  }
}