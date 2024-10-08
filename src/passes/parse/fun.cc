#include "../../miniml-lang.hh"
#include "../utils.hh"
#include "../internal.hh"


namespace miniml {

using namespace trieste;
  
inline const auto ungrouped_type = (type * type++);
inline const auto parenthesized_type = T(Paren) << ((T(Group) << ungrouped_type) / ungrouped_type);
inline const auto type_ann = ungrouped_type / parenthesized_type;
inline const auto typed_param = T(Paren) << (T(Group) << (T(Ident)[Ident] * T(Colon) * (type_ann)[Ty1]));
inline const auto untyped_param = T(Ident)[Ident];
  
PassDef fun() {
    return 
    {
      "fun",
      parse::wf_fun,
      dir::topdown,
      { 
        // Parameter/pattern of fundecl
       T(Fun) 
        * T(Ident)[Name] //function name 
        * (typed_param/untyped_param) 
        * (~T(Colon) * ~type_ann[Ty2]) //optional return type annotation
        * T(Is)       
        * Any++[Rhs] >>  
          [](Match& _) {  
            const auto expr = Expr << _[Rhs];
            const Node param_ty = _(Ty1) ? (Annotation << _[Ty1]) : Annotation << none_type();
            const Node ret_ty = _(Ty2) ? (Annotation << _[Ty2]) : Annotation << none_type(); 
            return Fun << (FunDef 
                               << _(Name) 
                               << ret_ty
                               << (Param << _(Ident) << param_ty)
                               << expr);
       },
      --(In(Expr)) * T(Fun)[Fun] >>
        [](Match& _){
          return Expr << _(Fun);
      },
      T(Annotation) << (T(Paren)[Paren] 
                      << (type[Type] / (T(Group) << ungrouped_type[Type]))) >> 
        [](Match& _){
          return Annotation << _[Type];
      },
      // parse arrow types 
      In(Annotation) * type[Lhs] * T(TypeArrow)[TypeArrow] * type++[Rhs] >> 
        [](Match& _){
          return _(TypeArrow) << _(Lhs) << _(Rhs);
      },
      // errors 
      // function bindings must have a fun id, a pattern (unary functions are simply val)
      // followed by an expression
      In(Fun)++ * T(Let)[Let] >>
          [](Match& _) { 
            return err(_(Let),"let declarations are only allowed at top level (missing ;;?)"); 
       }, 
      In(Annotation) * (type * type * type++)[Type] >>
         [](Match& _) { 
            return err(_(Type), "invalid type"); 
       },
      T(Fun)[Fun] << --T(FunDef) >>
          [](Match& _) { 
            return err(_(Fun), "invalid function declaration"); 
       },
       T(FunDef)[Fun] << !T(Ident) >>
          [](Match& _) { 
            return err(_(Fun), "function binding lacks identifier"); 
       },
        In(FunDef)[Fun] << (T(Expr) << End) >>
          [](Match& _) { 
            return err(_(Fun), "empty rhs"); 
       },
       T(FunDef)[Fun] << --(T(Ident) * T(Annotation) * T(Param) * T(Expr)) >>
          [](Match& _) { 
            return err(_(Fun), "illegal function declaration"); 
       },
       --In(Annotation,TypeArrow) * type[Type] >>
          [](Match& _) { 
            return err(_(Type), "parse error"); 
       },
        T(Colon)[Colon] >>
          [](Match& _) { 
            return err(_(Colon), ": can only appear in functions to annotate types"); 
       },
      In(Group)[Group] * T(Is,Fun) >>
          [](Match& _) { 
            return err(_(Group), "parse error"); 
       },
      }
    };

  }

}