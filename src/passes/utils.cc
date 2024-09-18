#include "../miniml-lang.hh"

namespace miniml {
  using namespace trieste;
  using Subst = std::map<std::string,std::pair<Node,std::string>>;  

int counter = 0;

auto err(const NodeRange& r, const std::string& msg)
{
    return Error << (ErrorMsg ^ msg) << (ErrorAst << r);
}

auto err(Node node, const std::string& msg)
{
    return Error << (ErrorMsg ^ msg) << (ErrorAst << node);
}

std::string node_val(Node node){
    std::string text(node->location().view());
    return text;
}

std::string ty_err_msg(Node c){
  std::string msg = "Internal error";
  if ((c)->type() == TypError){
    auto src_exp = node_val(c); //Type error should have exactly 2 children
    auto t1 = ((c)->front())->str();
    auto t2 = ((c)->back())->str(); 
    msg = "Cannot match type " + t1 + " with type" + t2 + " in expression " + src_exp;
  }
  return msg; 
}


Node get_type(Node n){
  return n/Type/Type;
}


Node none_type(){
  return TNone ^ "placeholder";
}


Node fresh_tvar()
{
  counter++;
  return TVar ^ "#a"+std::to_string(counter);
}

Node tvars()
{
  return (TVars ^ "tvars"); 
}

Node bool_type()
{
  return (TBool ^ "bool"); 
}

Node int_type()
{
  return (TInt ^ "int"); 
}

Node arrow_type(Node ty1, Node ty2)
{
  return (TypeArrow ^ "->") << ty1->clone() << ty2->clone();
}

Node forall_type(Node tvars, Node ty)
{
  return (ForAllTy ^ "forall") << tvars << ty->clone(); 
}

Node get_base_type(Node node)
  {
    trieste::Token typ = node->type();
    if (typ == Int){
      return int_type(); 
    } else if (typ == True || node->type() == False){
      return bool_type();
    } else {
      return err(node, "cannot be typed"); 
    }
  }

  Node get_op_type(Node node)
  {
    trieste::Token typ = node->type();
    if (typ.in({Add,Mul,Sub})){
      return int_type(); 
    } else if (typ.in({LT,Equals})){
      return bool_type();
    } else {
      return none_type(); //Error! 
    }
  }

Node create_constraint(Token constr, Node ty1, Node ty2, std::string origin)
{
  return (constr ^ origin) << ty1->clone() << ty2->clone();  
}

Node eq_constraint(Node ty1, Node ty2, std::string origin){
  return create_constraint(EqConstr,ty1,ty2,origin);
}

Node eq_constraint(Node ty1, Node ty2, Node origin){
  return create_constraint(EqConstr,ty1,ty2,node_val(origin));
}

Node inst_constraint(Node ty1, Node ty2, Node origin){
  return create_constraint(InstConstr,ty1,ty2,node_val(origin));
}

Node gen_constraint(Node ty1, Node ty2, Node origin){
  return create_constraint(GenConstr,ty1,ty2,node_val(origin));
}
  // for convinient lifting to top 
  Node lift_constraint(Node n){
    return Lift << TopExpr << n; 
  }

  Node lift_constraints(std::vector<Node> nodes){
    Node lift_constraint = Lift << TopExpr; 
    for(Node n : nodes){
      lift_constraint << n;
    }
    return lift_constraint; 
}

bool in(std::string var, NodeIt from, NodeIt end){
  while(from != end){
    if(node_val(*from) == var) return true;
    from++;
  }
  return false;
}

bool in_type(std::string var, Node ty){
  auto it = ty->begin();
  while(it != ty->end()){
    if(node_val(*it) == var) return true;
    else if((*it)->type() == TypeArrow){
      in_type(var,*it);
    }
    it++;
  }
  return false;
}


bool subst_arrow(bool upd, Node arrow_ty, std::string var, Node subst_ty){
  for (auto it=arrow_ty->begin(); it != arrow_ty->end(); it++){
      if((*it)->type() != TypeArrow){
        if(var == node_val(*it)){ //the tyvar we want to replace 
          arrow_ty->replace(*it,subst_ty); //apply substitution
          upd = true;
        } 
      } else { //if nested arrow type, recurse 
          upd = subst_arrow(upd, *it,var,subst_ty);
      }
  }
  return upd;
}

bool subst_type(bool upd, Node ty, std::string var, Node substty){
  // substitute all variables matching the given type variable in ty with substty
  if (ty->type() == TypeArrow){//typearrow 
    upd = subst_arrow(upd, ty, var, substty);
  } else if (ty->type() == ForAllTy){ //type scheme
    auto bound_vars = ty/TVars;
    if(!in(var,bound_vars->begin(), bound_vars->end())){
      upd = subst_type(upd,ty/Type,var,substty);
    }
  } else if (ty->type() == TVar && node_val(ty) == var){
      (ty->parent())->replace(ty,substty);
      upd = true;
  }
  return upd;
}

void update_substmap(std::shared_ptr<Subst> subst, Node tyvar, Node subst_ty, std::string payload){
  auto var = node_val(tyvar);
  (*subst)[var] = std::make_pair(subst_ty,payload); //add substitution
  for (auto [v,val] : *subst){
    auto [t,p] = val;
    if (t->type() != TypeArrow){
      if (var == node_val(t)){
        (*subst)[v] = std::make_pair(subst_ty,p);
      }
    } else {
      subst_arrow(true, t, var, subst_ty->clone());
      (*subst)[v] = std::make_pair(t, p);
    }
  }
}

bool apply_subst(std::shared_ptr<Subst> subst, Node constraint) {
    auto t1 = constraint/Ty1;
    auto t2 = constraint/Ty2;
    bool updated = false;
    for (auto [var,typ] : *subst){
      updated = subst_type(updated, t1, var, typ.first->clone()); //if tyvar found in t1, replace with substty
      updated = subst_type(updated, t2, var, typ.first->clone());
    }
  return updated;
}


void generalize_(Node ty, Node tvars){
  for (auto it = ty->begin();
            it != ty->end();
            it++){
    if((*it)->type() == TVar){ 
      if (!in(node_val(*it), tvars->begin(), tvars->end())){
        tvars << *it; 
      }
    } else if ((*it)->type() == TypeArrow){
      generalize_(*it,tvars);
    }
  }
}

Node generalize(Node ty1){
  const Node ty_vars = tvars();
  generalize_(ty1,ty_vars);
  return (ForAllTy ^ "forall") << ty_vars << (Type << ty1->clone());
}

Node instantiate_(std::map<std::string,Node> substmap, Node typ){
for (auto it = typ->begin(); it != typ->end(); it++){
      auto subst = substmap.find(node_val(*it));
      if((*it)->type() == TypeArrow){
        instantiate_(substmap, *it);
      }
      else if(subst != substmap.end()){
        typ->replace(*it, (subst->second)->clone());
      }
    }
  return typ;  
}

Node instantiate(Node ty){
  if (ty->type() == ForAllTy){
    auto substmap = std::map<std::string,Node>(); //local substmap 
    Node ftvars = ty/TVars;
    // initialize concrete type variables
    for (auto it = ftvars->begin(); it != ftvars->end(); it++){
      auto freshtv = fresh_tvar();
      substmap[node_val(*it)] = freshtv;
    }
    // substitute fresh variables in type 
    Node typ = ty/Type/Type;
    return instantiate_(substmap,typ->clone());
  }
  else {
    return ty->clone();
  }
}
}


