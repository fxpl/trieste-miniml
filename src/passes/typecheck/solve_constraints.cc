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
      (dir::topdown),
      {
        T(EqConstr)[EqConstr] << (type[Ty1] * type[Ty2]) >> 
          [local_subst](Match& _){
            auto constr = _(EqConstr);
            auto source_str = node_val(constr);
            auto constr_updated = apply_subst(local_subst,constr); 
            auto t1 = constr/Ty1; auto t2 = constr/Ty2;
            if (t1->type() == TVar){
              if(in_type(node_val(t1),t2)){ //if type variable t1 is used inside t2 
                return (TypError ^ source_str) << t1 << t2;
              } else if (node_val(t1) == node_val(t2)){
                return nothing;
              } 
              update_substmap(local_subst, t1, t2, source_str);
            }
            else if (t2->type() == TVar)
            {
              if(in_type(node_val(t2),t1)){ //if type variable t2 is used inside t1
                return (TypError ^ source_str) << t2 << t1;
              } else if (node_val(t1) == node_val(t2)){
                return nothing;
              }
              update_substmap(local_subst, t2, t1, source_str);
            }
            else if (t1->type() == TypeArrow && t2->type() == TypeArrow)
            {
              auto c1 = eq_constraint(t1 / Ty1, t2 / Ty1, source_str);
              auto c2 = eq_constraint(t1 / Ty2, t2 / Ty2, source_str);
              return Seq << c1 << c2; 
            }
            else if (t1->type() != t2->type())
            {
              return (TypError ^ source_str) << t1 << t2;
            }
            return (constr_updated) ? constr : nothing; //return updated constraint to view next 
            
        },
        T(InstConstr)[InstConstr] << (type[Ty1] * T(TVar)[Ty2])  >>
        [local_subst, global_subst](Match& _){
            auto constr = _(InstConstr);
            apply_subst(local_subst,constr);
            auto t1 = constr/Ty1; auto t2 = constr/Ty2;
            auto res = global_subst->find(node_val(t2));
            if (res != global_subst->end()){
              auto typ = instantiate((res->second.first));
              update_substmap(local_subst, t1, typ, node_val(constr)); 
              return eq_constraint(t1, typ, node_val(constr)); //return updated constraint to view next 
            }
            else
              return err(t2,"internal error, could not find type");
        }, 
        T(GenConstr)[GenConstr] << (type[Ty1] * type[Ty2]) >>
        [local_subst, global_subst](Match& _){
            auto constr = _(GenConstr);
            apply_subst(local_subst,constr);
            auto t1 = constr/Ty1; auto t2 = constr/Ty2;
            auto typ = generalize(t2);
            // add to global substitution
            (*global_subst)[node_val(t1)] = std::make_pair(typ, node_val(_(GenConstr)));
            update_substmap(local_subst, t1, typ, node_val(constr));  // update local subst
            return nothing;
        },
        // substitute types 
        In(Let,Expr,TypError)++ * (T(TVar)[TVar]) >>
          [local_subst](Match& _){
            auto res = local_subst->find(node_val(_(TVar)));
            if (res != local_subst->end()){
              return (res->second.first)->clone();
            }
            return NoChange ^ _(TVar); // no substitution found (yet)
        },
        // error
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
  // clean-up after pass 
  tc.post([](Node n){
      auto program = n->front();
      for(auto ts = program->begin(); ts != program->end(); ts++){
        auto cs = *ts/Constraints;
        auto exp = *ts/Expr;
        // propagate type errors 
        for(auto c = cs->begin(); c != cs->end(); c++){
          auto msg = ty_err_msg(*c);
          exp->push_front(Error << (ErrorMsg ^ msg));
        }
        program->replace(*ts,exp); //replace top exprs with exprs 
      }
      return 0; });

  return tc;
  }
}