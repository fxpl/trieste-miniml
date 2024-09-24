#include "../internal.hh"
#include "../utils.hh"

namespace miniml
{

  using namespace trieste;
  using namespace wf::ops;

  inline const Node nothing = {};

  // solve constraints and substitute types
  PassDef solve_constraints()
  {
    std::shared_ptr<Subst> global_subst = std::make_shared<Subst>();
    std::shared_ptr<Subst> local_subst = std::make_shared<Subst>();

    PassDef tc = {
      "solve_constraints",
      check::wf_solve_constr,
      dir::topdown | dir::once,
      {
        // Start by substituting the constraint, regardless of the type
        T(EqConstr, InstConstr, GenConstr)[Constr] << (type[Ty1] * type[Ty2]) >>
          [local_subst](Match& _) {
            subst_type(_(Ty1), local_subst);
            subst_type(_(Ty2), local_subst);
            auto constr = (_(Constr)->type() == EqConstr? SubstEqConstr:
                           _(Constr)->type() == InstConstr? SubstInstConstr:
                           SubstGenConstr) ^ _(Constr);
            return Reapply << (constr << _(Ty1) << _(Ty2));
          },

        T(SubstEqConstr)[EqConstr] <<
            ((T(TypeArrow) << (type[Ty11] * type[Ty12])) *
             (T(TypeArrow) << (type[Ty21] * type[Ty22]))) >>
          [](Match& _) {
              auto c1 = (SubstEqConstr ^ _(EqConstr)) << _(Ty11) << _(Ty21);
              auto c2 = (SubstEqConstr ^ _(EqConstr)) << _(Ty21) << _(Ty22);
              return Reapply << c1 << c2;
          },

        T(SubstEqConstr)[EqConstr] << ((T(TVar)[TVar] * type[Type]) /
                                       (type[Type] * T(TVar)[TVar])) >>
          [local_subst](Match& _) {
            // TODO: source_str is not needed in substmap anymore!
            auto source_str = node_val(_(EqConstr));
            if(in_type(node_val(_(TVar)), _(Type))) {
                return (TypError ^ _(EqConstr)) << _(TVar) << _(Type);
            }
            update_substmap(local_subst, _(TVar), _(Type), source_str);
            return nothing;
          },

        T(SubstEqConstr)[EqConstr] << (type[Ty1] * type[Ty2]) >>
          [](Match& _) {
              if (_(Ty1)->type() != _(Ty2)->type()) {
                return (TypError ^ _(EqConstr)) << _(Ty1) << _(Ty2);
              }
              return nothing;
          },

        // TODO: Ty1 can only be a type variable
        T(SubstInstConstr)[InstConstr] << (type[Ty1] * T(TVar)[TVar])  >>
        [local_subst, global_subst](Match& _){
            auto res = global_subst->find(node_val(_(TVar)));
            if (res != global_subst->end()){
              auto typ = instantiate(res->second.first);
              update_substmap(local_subst, _(Ty1), typ, node_val(_(InstConstr)));
              return Reapply << ((SubstEqConstr ^ _(InstConstr)) << _(Ty1) << typ); //return updated constraint to view next
            }
            // TODO: This will be dropped?
            else {
              return err(_(TVar), "internal error, could not find type");
            }
        },

        // TODO: Ty1 is a type variable
        T(SubstGenConstr)[GenConstr] << (type[Ty1] * type[Ty2]) >>
        [local_subst, global_subst](Match& _){
            auto typ = generalize(_(Ty2));
            // add to global substitution
            (*global_subst)[node_val(_(Ty1))] = std::make_pair(typ, node_val(_(GenConstr)));
            update_substmap(local_subst, _(Ty1), typ, node_val(_(GenConstr)));  // update local subst
            return nothing;
        },

        // substitute types
        In(Let,Expr,TypError)++ * (T(TVar)[TVar]) >>
          [local_subst](Match& _) -> Node {
            auto res = local_subst->find(node_val(_(TVar)));
            if (res != local_subst->end()){
              return (res->second.first)->clone();
            }
            // TODO: We probably want to report an error if we don't have a substitution here
            return NoChange; // no substitution found (yet)
        },

        // error
        // TODO: Can this happen? In fuzzing?
        In(TypeArrow) * T(ForAllTy)[ForAllTy] >>
          [](Match& _){
          return err(_(ForAllTy), "internal error, invalid type substitution");
        }
    }
  };
  tc.pre(TopExpr, [local_subst](Node ){
    local_subst->clear(); //clear local subst for each top expr
    return 0;
  });

  // TODO: Pass that replaces TopExpr by its Expr (wf is wf). Replace TypError above with regular Errors

  // clean-up after pass
  // TODO: See if this can be done as a post instead
  tc.post([](Node n){
      auto program = n->front();
      for(auto ts = program->begin(); ts != program->end(); ts++){
        auto cs = *ts/Constraints;
        auto exp = *ts/Expr;
        // propagate type errors
        for(auto c = cs->begin(); c != cs->end(); c++){
          auto msg = ty_err_msg(*c);
          exp->push_front(Error << (ErrorMsg ^ msg) << *c);
        }
        program->replace(*ts,exp); //replace top exprs with exprs
      }
      return 0; });

  return tc;
  }
}
