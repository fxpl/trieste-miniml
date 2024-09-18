#pragma once
#include "../miniml-lang.hh"

namespace miniml
{
  using namespace ::trieste;
  using Subst = std::map<std::string, std::pair<Node, std::string>>;

  //a few helper patterns
  inline const auto expr_binOp = T(Mul,Add,Sub,Equals,LT);
  inline const auto expr_keywords = T(If,Then,Else);  
  inline const auto expr_const = T(Int,True,False);
  inline const auto type = T(TInt,TBool,TVar,TypeArrow); 

  Node err(const NodeRange &r, const std::string &msg);

  Node err(Node node, const std::string &msg);
  
  std::string ty_err_msg(Node n);

  std::string node_val(Node node);

  Node get_type(Node n);

  Node none_type();

  Node fresh_tvar();

  Node tvars();

  Node bool_type();

  Node int_type();

  Node arrow_type(Node ty1, Node ty2);
  
  Node forall_type(Node tvars, Node ty);

  Node get_base_type(Node node);

  Node get_op_type(Node node);

  Node create_constraint(Token constr, Node ty1, Node ty2, std::string origin);
  Node eq_constraint(Node ty1, Node ty2, std::string origin);
  Node eq_constraint(Node ty1, Node ty2, Node origin);
  Node inst_constraint(Node ty1, Node ty2, Node origin);
  Node gen_constraint(Node ty1, Node ty2, Node origin);
  

  Node lift_constraint(Node n);

  Node lift_constraints(std::vector<Node> nodes);

  bool in(std::string var, NodeIt from, NodeIt end);

  bool in_type(std::string var, Node ty);

  void update_substmap(std::shared_ptr<Subst> subst, Node tyvar, Node subst_ty, std::string payload);

  bool apply_subst(std::shared_ptr<Subst> subst, Node ty);

  Node generalize(Node ty1);

  Node instantiate(Node ty);

}
