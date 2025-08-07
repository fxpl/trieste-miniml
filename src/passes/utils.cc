#include "../llvm-lang.hh"
#include "../miniml-lang.hh"

namespace miniml {
  using namespace trieste;
  using Subst = std::map<std::string, Node>;

  int counter = 0;

  Node err(const NodeRange& r, const std::string& msg) {
    return Error << (ErrorMsg ^ msg) << (ErrorAst << r);
  }

  Node err(Node node, const std::string& msg) {
    return Error << (ErrorMsg ^ msg) << (ErrorAst << node);
  }

  std::string node_val(Node node) {
    std::string text(node->location().view());
    return text;
  }

  Node get_type(Node n) {
    return n / Type / Type;
  }

  Node none_type() {
    return TNone ^ "placeholder";
  }

  Node fresh_tvar() {
    counter++;
    return TVar ^ "#a" + std::to_string(counter);
  }

  Node tvars() {
    return (TVars ^ "tvars");
  }

  Node bool_type() {
    return (TBool ^ "bool");
  }

  Node int_type() {
    return (TInt ^ "int");
  }

  Node arrow_type(Node ty1, Node ty2) {
    return (TypeArrow ^ "->") << ty1->clone() << ty2->clone();
  }

  Node forall_type(Node tvars, Node ty) {
    return (ForAllTy ^ "forall") << tvars << ty->clone();
  }

  Node get_base_type(Node node) {
    trieste::Token typ = node->type();
    if (typ == Int) {
      return int_type();
    } else if (typ == True || node->type() == False) {
      return bool_type();
    } else {
      return err(node, "cannot be typed");
    }
  }

  Node get_op_type(Node node) {
    trieste::Token typ = node->type();
    if (typ.in({Add, Mul, Sub})) {
      return int_type();
    } else if (typ.in({LT, Equals})) {
      return bool_type();
    } else {
      return none_type(); // Error!
    }
  }

  Node create_constraint(Token constr, Node ty1, Node ty2, std::string origin) {
    return (constr ^ origin) << ty1->clone() << ty2->clone();
  }

  Node eq_constraint(Node ty1, Node ty2, std::string origin) {
    return create_constraint(EqConstr, ty1, ty2, origin);
  }

  Node eq_constraint(Node ty1, Node ty2, Node origin) {
    return create_constraint(EqConstr, ty1, ty2, node_val(origin));
  }

  Node inst_constraint(Node ty1, Node ty2, Node origin) {
    return create_constraint(InstConstr, ty1, ty2, node_val(origin));
  }

  Node gen_constraint(Node ty1, Node ty2, Node origin) {
    return create_constraint(GenConstr, ty1, ty2, node_val(origin));
  }

  // for convinient lifting to top
  Node lift_constraint(Node n) {
    return Lift << TopExpr << n;
  }

  Node lift_constraints(std::vector<Node> nodes) {
    Node lift_constraint = Lift << TopExpr;
    for (Node n : nodes) {
      lift_constraint << n;
    }
    return lift_constraint;
  }

  bool contains_var(std::string var, Node ty) {
    for (auto child : *ty) {
      if (child->type() == TVar && node_val(child) == var)
        return true;
    }
    return false;
  }

  bool in_type(std::string var, Node ty) {
    for (auto child : *ty) {
      if (child->type() == TVar && node_val(child) == var)
        return true;
      else if (child->type() == TypeArrow) {
        in_type(var, child);
      }
    }
    return false;
  }

  void subst_type(
    Node ty, std::shared_ptr<Subst> subst, std::multiset<std::string>& bound) {
    if (ty->type() == TVar) {
      auto name = node_val(ty);
      if (
        subst->find(name) != subst->end() && bound.find(name) == bound.end()) {
        ty->parent()->replace(ty, (*subst)[name]->clone());
      }
    } else if (ty->type() == TypeArrow) {
      for (auto& child : *ty) {
        subst_type(child, subst, bound);
      }
    } else if (ty->type() == ForAllTy) {
      auto params = ty / TVars;
      for (auto param : *params) {
        auto param_name = node_val(param);
        bound.insert(param_name);
      }
      subst_type(ty / Type, subst, bound);
      for (auto param : *params) {
        auto param_name = node_val(param);
        bound.erase(param_name);
      }
    }
  }

  void subst_type(Node ty, std::shared_ptr<Subst> subst) {
    std::multiset<std::string> bound;
    subst_type(ty, subst, bound);
  }

  void
  update_substmap(std::shared_ptr<Subst> subst, Node tyvar, Node subst_ty) {
    auto var = node_val(tyvar);
    (*subst)[var] = subst_ty->clone(); // add substitution
    for (auto [v, t] : *subst) {
      if (t->type() == TVar) {
        if (var == node_val(t)) {
          (*subst)[v] = subst_ty->clone();
        }
      } else {
        subst_type(t, subst);
      }
    }
  }

  void generalize_(Node ty, Node tvars) {
    for (auto child : *ty) {
      if (child->type() == TVar) {
        if (!contains_var(node_val(child), tvars)) {
          tvars << child->clone();
        }
      } else if (child->type() == TypeArrow) {
        generalize_(child, tvars);
      }
    }
  }

  Node generalize(Node ty1) {
    const Node ty_vars = tvars();
    generalize_(ty1, ty_vars);
    return (ForAllTy ^ "forall") << ty_vars << (Type << ty1->clone());
  }

  Node instantiate_(Subst substmap, Node typ) {
    for (auto child : *typ) {
      auto subst = substmap.find(node_val(child));
      if (child->type() == TypeArrow) {
        instantiate_(substmap, child);
      } else if (subst != substmap.end()) {
        typ->replace(child, (subst->second)->clone());
      }
    }
    return typ;
  }

  Node instantiate(Node ty) {
    if (ty->type() == ForAllTy) {
      auto substmap = Subst(); // local substmap
      Node ftvars = ty / TVars;
      // initialize concrete type variables
      for (auto child : *ftvars) {
        auto freshtv = fresh_tvar();
        substmap[node_val(child)] = freshtv;
      }
      // substitute fresh variables in type
      Node typ = ty / Type / Type;
      return instantiate_(substmap, typ->clone());
    } else {
      return ty->clone();
    }
  }

  Node getLLVMType(Node type) {
    if (type == TInt) {
      return llvmir::Ti32;
    } else if (type == TBool) {
      return llvmir::Ti1;
    } else if (type == TypeArrow) {
      return llvmir::TPtr;
    } else if (type == TPtr) {
      return llvmir::TPtr;
    } else {
      return nullptr;
    }
  }

  Node pop_front(Node node) {
    // FIXME: Children are stored in a vector, pop-ing from front is O(n).
    if (node->empty()) {
      return {};
    }

    // Keeps reference alive.
    Node child = node->front()->clone();
    node->erase(node->begin(), node->begin() + 1);

    return child;
  }
}
