#include "../miniml-lang.hh"

namespace miniml {
  using namespace trieste;
  using Subst = std::map<std::string, Node>;

int counter = 0;

Node err(const NodeRange& r, const std::string& msg)
{
    return Error << (ErrorMsg ^ msg) << (ErrorAst << r);
}

Node err(Node node, const std::string& msg)
{
    return Error << (ErrorMsg ^ msg) << (ErrorAst << node);
}

std::string node_val(Node node){
    std::string text(node->location().view());
    return text;
}

std::string ty_err_msg(Node c){
  std::string msg;
  if ((c)->type() == TypError){
    auto src_exp = node_val(c); // Type error should have exactly 2 children
    auto t1 = ((c)->front())->str();
    auto t2 = ((c)->back())->str();
    msg = "Cannot match type " + t1 + " with type" + t2 + " in expression " + src_exp;
  } else {
    msg = "Internal error: Unexpected " + c->str();
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
  // TODO: For loop?
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

void subst_type(Node ty, std::shared_ptr<Subst> subst, std::vector<std::string>& bound) {
  if (ty->type() == TVar) {
      auto name = node_val(ty);
      // TODO: Check bound names!
      if (subst->find(name) != subst->end()) {
          ty->parent()->replace(ty, (*subst)[name]->clone());
      }
  } else if (ty->type() == TypeArrow) {
      for (auto& child : *ty) {
          subst_type(child, subst, bound);
      }
  } else if (ty->type() == ForAllTy) {
      auto params = ty/TVars;
      for (auto param : *params) {
        auto param_name = node_val(param);
        bound.push_back(param_name);
      }
      subst_type(ty/Type, subst, bound);
      for (auto param : *params) {
        bound.pop_back();
      }
  }
}

void subst_type(Node ty, std::shared_ptr<Subst> subst) {
  std::vector<std::string> bound;
  subst_type(ty, subst, bound);
}

void update_substmap(std::shared_ptr<Subst> subst, Node tyvar, Node subst_ty) {
  auto var = node_val(tyvar);
  (*subst)[var] = subst_ty->clone(); // add substitution
  for (auto [v,t] : *subst) {
    if (t->type() == TVar) {
      if (var == node_val(t)) {
          (*subst)[v] = subst_ty->clone();
      }
    } else {
      subst_type(t, subst);
    }
  }
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


