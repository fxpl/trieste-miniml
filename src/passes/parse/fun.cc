#include "../../miniml-lang.hh"
#include "../utils.hh"
#include "../internal.hh"


namespace miniml {

using namespace trieste;

// Tokens that may appear in a not-yet-parsed type annotation. Parenthesized
// sub-types are still Paren nodes at this point.
inline const auto type_tok = T(TInt, TBool, TVar, TypeArrow, Paren);
inline const auto type_ann = type_tok * type_tok++;

// The TypeArrow token is used both for the unparsed `->` (no children) and for
// a parsed arrow type (two children).
inline const auto raw_arrow = T(TypeArrow) << End;

// A single (possibly not yet fully parsed) type: a primitive, a parsed arrow, a
// parenthesized type, or a nested annotation that is still being parsed.
inline const auto type_unit =
  T(TInt, TBool, TVar, Paren, Annotation) / (T(TypeArrow) << Any);

PassDef fun() {
  return {
    "fun",
    parse::wf_fun,
    dir::topdown,
    {
      // Parameter/pattern of fundecl. Both annotations must extend to the end
      // of their group, so that no trailing type tokens are silently dropped.
      (T(Fun)
       << (T(Group)
           << (T(Ident)[Name] // function name
               * (T(Ident)[Ident] /
                  (T(Paren)
                   << (T(Group)
                       << (T(Ident)[Ident] * T(Colon) * type_ann[Ty1] *
                           End)))) // parameter
               * ~(T(Colon) * type_ann[Ty2]) // optional return type annotation
               * End)))
          * (T(Is) << T(Group)[Is]) >>
        [](Match& _) -> Node {
        const auto expr = Expr << *_[Is];
        const Node param_ty =
          (_(Ty1) ? Annotation << _[Ty1] : Annotation << none_type());
        const Node ret_ty =
          (_(Ty2) ? Annotation << _[Ty2] : Annotation << none_type());
        return Fun
          << (FunDef << _(Name) << ret_ty << (Param << _(Ident) << param_ty)
                     << expr);
      },

      --(In(Expr)) * T(Fun)[Fun] >>
        [](Match& _) -> Node { return Expr << _(Fun); },

      // Parse arrow types (right associative): `t1 -> rest`. The right-hand
      // side is parsed in its own nested Annotation. Listed before the Paren
      // rule so that a parenthesized left-hand side is consumed together with
      // its arrow.
      In(Annotation) * type_unit[Lhs] * raw_arrow * (Any * Any++)[Rhs] >>
        [](Match& _) {
          return TypeArrow << _(Lhs) << (Annotation << _[Rhs]);
        },

      // A parenthesized type is parsed as a nested Annotation
      In(Annotation, TypeArrow) *
          (T(Paren) << ((T(Group) << (Any * Any++)[Type]) / (Any * Any++)[Type])) >>
        [](Match& _) { return Annotation << _[Type]; },

      // A nested Annotation that has been parsed down to a single type is
      // replaced by that type
      In(Annotation, TypeArrow) * (T(Annotation) << (type_unit[Type] * End)) >>
        [](Match& _) { return _(Type); },

      // parse print primitive
      --In(Expr) * T(Print)[Print] >>
        [](Match& _) -> Node { return Expr << _(Print); },

      // errors
      // function bindings must have a fun id, a pattern (unary functions are
      // simply val)
      // followed by an expression

      // Only states that cannot be parsed further are errors: two types next
      // to each other, an arrow without a type on its left (a well-formed
      // arrow is consumed by the arrow rule at its left-hand side), an empty
      // pair of parentheses, or a non-type token inside a type.
      In(Annotation) * (type_unit * type_unit)[Type] >>
        [](Match& _) { return err(_(Type), "invalid type"); },

      In(Annotation) * raw_arrow[Type] >>
        [](Match& _) { return err(_(Type), "invalid type"); },

      In(Annotation, TypeArrow) * (T(Paren) << End)[Type] >>
        [](Match& _) { return err(_(Type), "invalid type"); },

      In(Annotation) * (!(type_tok / T(Annotation, TNone)))[Type] >>
        [](Match& _) { return err(_(Type), "invalid type"); },

      T(Fun)[Fun] << --T(FunDef) >>
        [](Match& _) { return err(_(Fun), "invalid function declaration"); },

      In(FunDef)[FunDef] * (T(Expr) << End) >>
        [](Match& _) { return err(_(FunDef), "empty function body"); },

      --In(Annotation, TypeArrow) * type[Type] >>
        [](Match& _) { return err(_(Type), "parse error"); },

      T(Colon)[Colon] >>
        [](Match& _) {
          return err(
            _(Colon), ": can only appear in functions to annotate types");
        },

      In(Group) * T(Is, Fun)[Fun] >>
        [](Match& _) { return err(_(Fun), "parse error"); },
    }};
  }

}
