#include "../internal.hh"
#include "../utils.hh"

//Infer types and constraints

namespace miniml
{

  using namespace trieste;
  using namespace wf::ops;

  // Give function parameters and let declarations fresh type variables
  PassDef inf_fresh()
  {
    return {
        "inf_fresh",
        check::wf_fresh,
        (dir::bottomup),
        {
            // Create constraints for annotated params and introduce a new type variable
            T(Param)[Param] << (T(Ident)[Ident] * T(Annotation)[Annotation]) >>
                [](Match &_)
                {
                  auto fresh_ty = fresh_tvar();
                  auto pat = Param << _(Ident) << (Type << fresh_ty);
                  return (_(Annotation) / Type == TNone)
                             ? pat // no annotation
                             : Seq << lift_constraint(eq_constraint(fresh_ty, _(Annotation) / Type, _(Param)))
                                   << pat;
                },
            T(Let)[Let] << (T(Ident)[Ident] * T(Expr)[Expr]) >>
                [](Match &_)
                {
                  return (Let << _(Ident)
                              << (Type << fresh_tvar())
                              << _(Expr));
                },
            // Create constraint for function return type annotation
            T(FunDef)[FunDef] << (T(Ident)[Ident] * T(Annotation)[Annotation] * T(Param)[Param] * T(Expr)[Expr]) >>
                [](Match &_)
                {
                  auto arg_ty = get_type(_(Param));
                  auto ret_ty = (_(Annotation) / Type);
                  auto ret_vty = fresh_tvar();
                  auto fun_ty = arrow_type(arg_ty, ret_vty)->clone();
                  auto fundef = FunDef << _(Ident) << (Type << fun_ty) << _(Param) << _(Expr);
                  // Create constraints with annotated types
                  if (!(ret_ty->type() == TNone))
                  {
                    auto constraint = eq_constraint(ret_vty, ret_ty, _(FunDef));
                    return Seq << lift_constraint(constraint) << fundef;
                  }
                  else
                  {
                    return fundef;
                  }
                },
            // Group constraints
            T(TopExpr) << (T(EqConstr, InstConstr)++[Constraints] * T(Let, Expr)[Expr]) >>
                [](Match &_)
                {
                  return TopExpr << (Constraints << _[Constraints])
                                 << _(Expr);
                },
            In(TopExpr) * (T(Constraints)[Constraints] * (T(EqConstr, InstConstr) * T(EqConstr, InstConstr)++)[EqConstr]) >>
                [](Match &_)
                {
                  return (_(Constraints) << _[EqConstr]);
                },
            // Fuzz error 
            T(Type) << T(TNone)[TNone] >>
                [](Match &_)
                {
                  return err(_(TNone), "missing type annotation");
                },
        }};
  }

  // Infer expression types and generate constraints
  PassDef inf_exprs()
  {
    PassDef ie = {
        "inf_exprs",
        check::wf_inf_exprs,
        (dir::bottomup),
        {
            // Constant expression
            T(Expr) << expr_const[Expr] >>
                [](Match &_)
                {
                  auto exp = _(Expr);
                  auto typ = get_base_type(exp);
                  return Expr << (Type << typ) << exp;
                },
            // Identifier expression
            T(Expr)[Expr] << T(Ident)[Ident] >>
                [](Match &_)
                {
                  // lookup bound pattern/let usage/fun
                  Nodes bind = _(Ident)->lookup();
                  if (bind.size() != 0)
                  {
                    auto bound_node = (bind.back())->clone(); // use latest definition of ident
                    auto type = bound_node/Type;
                    if (bound_node->type() == Let)
                    { // if let bound ident, create "instance of" constraint
                      auto fresh = fresh_tvar();
                      auto inst_constr = inst_constraint(fresh, type/Type, _(Ident));
                      return Seq << lift_constraint(inst_constr)
                                 << (Expr << (Type << fresh) << _(Ident));
                    }
                    else
                      return Expr << type << _(Ident);
                  }
                  else
                  {
                    return err(_(Expr), "unbound variable " + node_val(_(Ident)));
                  }
                },
            // Conditional
            T(Expr)[Lhs] << (T(If)[If] << ((T(Expr) << T(Type)[Expr]) * (T(Expr) << T(Type)[Then]) * (T(Expr) << T(Type)[Else]))) >>
                [](Match &_)
                {
                  auto fresh_typ = fresh_tvar();
                  auto exp_typ = _(Expr) / Type;
                  auto then_typ = _(Then) / Type;
                  auto exp_is_bool = eq_constraint(exp_typ, bool_type(), _(If));
                  auto ifelse_eq = eq_constraint(then_typ, (_(Else) / Type), _(If));
                  auto ret_eq = eq_constraint(fresh_typ, then_typ, _(If));

                  return Seq << (lift_constraints({exp_is_bool, ifelse_eq, ret_eq})) // lift constraints
                             << (Expr << (Type << fresh_typ) << _(If));
                },
            // Binary operator
            T(Expr)[Expr] << (expr_binOp[Op]
                            << ((T(Expr) << (T(Type)[Lhs] * Any)) * (T(Expr) << (T(Type)[Rhs] * Any)))) >>
                [](Match &_)
                {
                  auto optyp = get_op_type(_(Op));
                  auto l_typ = _(Lhs) / Type;
                  auto r_typ = _(Rhs) / Type;
                  auto op_eqconstraint = eq_constraint(l_typ, r_typ, _(Expr));
                  auto expr = (Expr << (Type << optyp) << _(Op));
                  return (_(Op)->type() == Equals)
                    ? Seq << (lift_constraint(op_eqconstraint)) << expr
                    : Seq << (lift_constraints({op_eqconstraint, eq_constraint(r_typ, int_type(), _(Expr))}))
                          << expr;
                },
            // Function definition 
            T(Expr) << (T(Fun)[Fun]
                        << (T(FunDef)[FunDef] << (T(Ident)[Ident] * (T(Type)[TypeArrow]) * T(Param)[Param] * (T(Expr)[Expr] << T(Type)[Type])))) >>
                [](Match &_)
                {
                  auto fun_ty = _(TypeArrow) / Type;
                  if (fun_ty->type() != TypeArrow)
                  {
                    return err(fun_ty, "internal error: expected function type");
                  }
                  auto ret_ty = fun_ty / Ty2;
                  auto exp_ty = _(Type) / Type;
                  auto expty_eq_retty = eq_constraint(exp_ty, ret_ty, _(Fun));
                  return Seq << lift_constraint(expty_eq_retty)
                             << (Expr << (Type << fun_ty->clone()) << _(Fun));
                },
            // Function application
            T(Expr)[Expr] << (T(App)[App]
                              << ((T(Expr) << T(Type)[Lhs]) * (T(Expr) << T(Type)[Rhs]))) >>
                [](Match &_)
                {
                  auto lhs_typ = _(Lhs) / Type;
                  auto rhs_typ = _(Rhs) / Type;
                  auto ret_typ = fresh_tvar();
                  auto expr = Expr << (Type << ret_typ) << _(App);
                  if (lhs_typ->type() == TypeArrow)
                  {
                    auto ret_constraint = eq_constraint(lhs_typ / Ty2, ret_typ, _(App));
                    auto fun_constraint = eq_constraint(lhs_typ / Ty1, rhs_typ, _(App));
                    return Seq << lift_constraints({fun_constraint, ret_constraint})
                               << expr;
                  }
                  else if (lhs_typ->type() == TVar)
                  {
                    auto fun_typ = arrow_type(rhs_typ, ret_typ);
                    auto fun_constraint = eq_constraint(lhs_typ, fun_typ, _(App));
                    return Seq << lift_constraint(fun_constraint)
                               << expr;
                  }
                  else
                  {
                    auto errmsg = "invalid function application, expected a function type, got " + node_val(lhs_typ) + " in expr " + node_val(_(Lhs));
                    return err(_(Expr), errmsg);
                  }
                },
            // Group constraints
            T(TopExpr) << (T(EqConstr, InstConstr)++[Constraints] * T(Let, Expr)[Expr]) >>
                [](Match &_)
                {
                  return TopExpr << (Constraints << _[Constraints])
                                 << _(Expr);
                },
            In(TopExpr) * (T(Constraints)[Constraints] * (T(EqConstr, InstConstr) * T(EqConstr, InstConstr)++)[EqConstr]) >>
                [](Match &_)
                {
                  return (_(Constraints) << _[EqConstr]);
                },
        }};
    return ie;
  }

  PassDef let_constr()
  {
    return {
        "let_constr",
        check::wf_inf_exprs,
        (dir::once),
        {// generate generalization constraints for let-declaration
         In(TopExpr) * (T(Constraints)[Constraints] * T(Let)[Let]) >>
         [](Match &_)
         {
           auto let_typ = get_type(_(Let));
           auto rhs_typ = get_type(_(Let) / Expr);
           if (let_typ->type() != TVar) {
             return err(_(Let), "Internal error: let binding has non-variable type");
           }
           auto constr = gen_constraint(let_typ, rhs_typ, _(Let));
           return Seq << (_(Constraints) << constr)
                      << _(Let);
         }}};
  }
}
