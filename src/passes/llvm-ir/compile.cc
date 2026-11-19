#include "../../llvm-lang.hh"
#include "../../miniml-lang.hh"
#include "../internal.hh"
#include "../utils.hh"
#include "trieste/token.h"
#include <unordered_map>

namespace miniml {

  using namespace trieste;

  struct CompileContext {
    // Maps the name of each top-level `let`-bound identifier to its true
    // declared type, i.e. the type before instantiation. For a
    // polymorphic binding this still contains bare TVars. A call site only
    // carries the locally-instantiated (concrete) type of a `Global`
    // reference, which can hide that the callee's physical signature still
    // expects a boxed (Ti64) value at that position - this map is how that
    // is recovered. Populated once, up front, in the program-entry rule.
    std::unordered_map<std::string, Node> globalTypes;

    ~CompileContext() {}
  };

  // Finds every top-level `let` binding (the only place `Let` nodes occur -
  // MiniML has no local/expression-level `let`) and records its declared
  // type, stripped of the outer `ForAllTy` scheme so a polymorphic
  // binding's type reads e.g. as `TVar -> TVar` rather than
  // `forall a. a -> a`.
  void collect_global_types(
    Node n, std::unordered_map<std::string, Node>& out) {
    for (auto child : *n) {
      if (child->type() == Let) {
        Node ident = child / Ident;
        out[node_val(ident)] = get_type(child / Type);
      } else {
        collect_global_types(child, out);
      }
    }
  }

  // Converts a value from its natural LLVM representation into the
  // uniform boxed word (Ti64) used wherever a polymorphic function's
  // declared signature says TVar.
  Node box_value(Node srcId, Node dstId, Node srcType) {
    Node op = (srcType == llvmir::TPtr) ? llvmir::PtrToInt : llvmir::ZExt;
    return (llvmir::Instr
            << (llvmir::ConversionOp
                << (op << dstId << srcId << llvmir::Ti64)));
  }

  // Converts a boxed word (Ti64) back into a value's natural LLVM
  // representation, at a call site that statically knows the concrete
  // type a polymorphic result was instantiated to.
  Node unbox_value(Node srcId, Node dstId, Node dstType) {
    Node op = (dstType == llvmir::TPtr) ? llvmir::IntToPtr : llvmir::Trunc;
    return (llvmir::Instr
            << (llvmir::ConversionOp
                << (op << dstId << srcId << dstType->clone())));
  }

  /**
   * @brief
   *
   * It works similar to denotational semantics where each miniML
   * expression is mapped to its meaning in LLVM IR.
   *
   * [[_]]t : miniML -> LLVM IR
   *
   * [[e1 + e2]]t0 =
   *  [[e1]]t1 : [[e2]]t2 =
   *    add i32 [[e1]]t1, [[e2]]t2
   *
   */
  PassDef compile() {
    auto context = std::make_shared<CompileContext>();

    return {
      "compile",
      LLVMIRCompilation::wf,
      (dir::topdown),
      {
        /**
         * Add compile node and assign tmp/register.
         */
        In(Top) * Start * T(IRProgram)[Program] * End >>
          [context](Match& _) -> Node {
          collect_global_types(_(Program), context->globalTypes);
          auto children = *_[Program];
          return (Compile << (llvmir::Program << children));
        },

        /**
         * Compile program and add compile nodes to its children.
         */
        T(Compile) << T(llvmir::Program)[Program] >> [](Match& _) -> Node {
          Node prog = _(Program);
          Node new_prog = llvmir::Program;

          for (auto topexpr : *prog) {
            if (topexpr == TopExpr || topexpr == Expr) {
              Node ident = llvmir::Ident ^ prog->fresh();
              new_prog->push_back(Compile << ident << topexpr);
            } else {
              new_prog->push_back(Compile << topexpr);
            }
          }

          return new_prog;
        },

        /**
         * Compile TopExpression.
         */
        T(Compile) << T(llvmir::Ident)[Result] *
              (T(TopExpr) << T(Expr, Let)[Expr]) >>
          [](Match& _) -> Node { return (Compile << _(Result) << _(Expr)); },

        /**
         * Let
         */
        T(Compile) << T(llvmir::Ident)[Result] * T(Let)[Let] >>
          [context](Match& _) -> Node {
          // TODO: Need to deal with ForAllTy. Until I figure out how, just
          // pretend it cannot be nested and ignore it.
          Node let = _(Let);
          Node ident = let / Ident;
          Node expr = let / Expr;
          Node type = get_type(let / Type);

          Node llvmType = getLLVMType(type);
          if (llvmType == nullptr) {
            return err(_(Type), "let type not supported");
          }

          Node irIdent = llvmir::Ident ^ node_val(ident);

          return Seq << (llvmir::Instr
                         << (llvmir::MemoryOp
                             << (llvmir::Alloca << irIdent << llvmType)))
                     << (Compile << _(Result) << expr)
                     << (llvmir::Instr
                         << (llvmir::MemoryOp
                             << (llvmir::Store << _(Result)->clone()
                                               << irIdent->clone())));
        },

        /**
         * Boolean
         */
        T(Compile) << T(llvmir::Ident)[Result] *
              (T(Expr) << (T(Type) << T(TBool)[Type]) *
                 T(True, False)[TBool]) >>
          [](Match& _) -> Node {
          Node irValue = _(TBool);
          Node value = irValue == True? llvmir::IRValue ^ "1":
                       irValue == False? llvmir::IRValue ^ "0":
                       err(irValue, "malformed boolean constant");

          Node llvmType = getLLVMType(_(Type));

          return llvmir::Action
            << (llvmir::CreateConst << _(Result) << llvmType << value);
        },

        /**
         * Integer
         */
        T(Compile) << T(llvmir::Ident)[Result] *
              (T(Expr) << (T(Type) << T(TInt)[Type]) * T(Int)[Int]) >>
          [](Match& _) -> Node {
          Node value = llvmir::IRValue ^ node_val(_(Int));

          Node llvmType = getLLVMType(_(Type));

          return llvmir::Action
            << (llvmir::CreateConst << _(Result) << llvmType << value);
        },

        /**
         * Identifier
         */
        T(Compile)
            << (T(llvmir::Ident)[Result] *
                (T(Expr) << (T(Type)[Type] * T(Ident)[Ident]))) >>
          [context](Match& _) -> Node {
          auto llvmType = getLLVMType(_(Type) / Type);

          Node irIdent = llvmir::Ident ^ node_val(_(Ident));

          return llvmir::Instr
            << (llvmir::ConversionOp
                << (llvmir::BitCast << _(Result) << irIdent << llvmType));
        },

        /**
         * Global
         */
        T(Compile)
            << (T(llvmir::Ident)[Result] *
                (T(Expr) << (T(Type)[Type] * T(Global)[Global]))) >>
          [context](Match& _) -> Node {
          auto llvmType = getLLVMType(_(Type) / Type);

          return llvmir::Instr
            << (llvmir::MemoryOp
                << (llvmir::Load << _(Result) << llvmType
                                 << (llvmir::Ident ^ node_val(_(Global)))));
        },

        /**
         * Binary operation.
         */
        T(Compile) << T(llvmir::Ident)[Result] *
              (T(Expr) << T(Type)[Type] * T(Add, Sub, Mul)[Op]) >>
          [](Match& _) -> Node {
          Node resultId = _(Result);
          Node binop = _(Op);
          Node lhs = binop / Lhs;
          Node rhs = binop / Rhs;

          Node op = binop == Add ? llvmir::Add
                  : binop == Sub ? llvmir::Sub
                  : binop == Mul ? llvmir::Mul
                  : err(binop, "malformed binary operator");

          Node llvmType = getLLVMType(_(Type) / Type);
          if (llvmType != llvmir::Ti32) {
            return err(_(Type), "arithmetic on non-int type");
          }

          Node lhsId = llvmir::Ident ^ lhs->fresh();
          Node rhsId = llvmir::Ident ^ rhs->fresh();

          // Instruction
          return Seq << (Compile << lhsId->clone() << lhs)
                     << (Compile << rhsId->clone() << rhs)
                     << (llvmir::Instr
                         << (llvmir::BinaryOp
                             << (op << resultId << llvmType << lhsId
                                    << rhsId)));
        },

        /**
         * Comparison
         */
        T(Compile) << T(llvmir::Ident)[Result] *
              (T(Expr) << T(Type) * T(Equals, LT)[Op]) >>
          [](Match& _) -> Node {
          Node comparison = _(Op);
          Node lhs = comparison / Lhs;
          Node rhs = comparison / Rhs;

          if (get_type(lhs)->type() != get_type(rhs)->type()) {
            return err(comparison, "comparison operands have different types");
          }

          Node type = get_type(lhs);
          Node llvmType = getLLVMType(type);
          // The type checker only allows `=` on types that admit equality
          // (int, bool and equality type variables) and `<` on int.
          // Polymorphic values are compared as boxed words, which is exact
          // since boxing an int or a bool is injective (see box_value).
          bool supported = (comparison == Equals)
            ? llvmType->type().in({llvmir::Ti1, llvmir::Ti32, llvmir::Ti64})
            : llvmType == llvmir::Ti32;
          if (!supported) {
            return err(lhs / Type, "comparison type not supported");
          }

          Node op = NULL;
          if (comparison == Equals) {
            op = llvmir::EQ;
          } else if (comparison == LT) {
            op = llvmir::SLT;
          } else {
            return err(comparison, "comparison operator not supported");
          }

          Node lhsIdent = llvmir::Ident ^ lhs->fresh();
          Node rhsIdent = llvmir::Ident ^ rhs->fresh();

          return Seq << (Compile << lhsIdent->clone() << lhs)
                     << (Compile << rhsIdent->clone() << rhs)
                     << (llvmir::Instr
                         << (llvmir::MiscOp
                             << (llvmir::Icmp << _(Result) << op << llvmType
                                              << lhsIdent << rhsIdent)));
        },

        /**
         * If-then-else
         */
        T(Compile) << T(llvmir::Ident)[Result] *
              (T(Expr)[Expr] << T(Type) *
                 (T(If) << T(Expr)[Cond] * T(Expr)[True] * T(Expr)[False])) >>
          [](Match& _) -> Node {
          Node type = get_type(_(Expr));
          if (type == nullptr) {
            return err(_(Type), "if-then-else type not supported");
          }
          Node llvmType = getLLVMType(type);

          Node condId = llvmir::Ident ^ _(Cond)->fresh();
          Node ifTrueId = llvmir::Ident ^ _(True)->fresh();
          Node ifFalseId = llvmir::Ident ^ _(False)->fresh();

          std::string ifId = node_val(condId);
          Node thenLabel = llvmir::Label ^ ("then" + ifId);
          Node thenEndLabel = llvmir::Label ^ ("thenEnd" + ifId);
          Node elseLabel = llvmir::Label ^ ("else" + ifId);
          Node elseEndLabel = llvmir::Label ^ ("elseEnd" + ifId);
          Node ifEndLabel = llvmir::Label ^ ("ifEnd" + ifId);

          // clang-format off
          return Seq
            << (Compile << condId << _(Cond))
            << (llvmir::TerminatorOp
                << (llvmir::Branch
                    << condId->clone()
                    << thenLabel->clone()
                    << elseLabel->clone()))
            << (thenLabel)
            << (Compile << ifTrueId->clone() << _(True))
            << (llvmir::TerminatorOp
                << (llvmir::Jump << thenEndLabel->clone()))
            // "Landing" block so Phi unaffected by branching in Then/Else expr.
            << (thenEndLabel)
            << (llvmir::TerminatorOp
                << (llvmir::Jump << ifEndLabel->clone()))
            << (elseLabel)
            << (Compile << ifFalseId->clone() << _(False))
            << (llvmir::TerminatorOp
                << (llvmir::Jump << elseEndLabel->clone()))
            // "Landing" block so Phi unaffected by branching in Then/Else expr.
            << (elseEndLabel)
            << (llvmir::TerminatorOp
                << (llvmir::Jump << ifEndLabel->clone()))
            << (ifEndLabel)
            << (llvmir::Instr
                << (llvmir::MiscOp
                    << (llvmir::Phi
                        << _(Result)
                        << llvmType
                        << (llvmir::PredecessorList
                            << (llvmir::Predecessor << ifTrueId << thenEndLabel->clone())
                            << (llvmir::Predecessor << ifFalseId << elseEndLabel->clone())))));
          // clang-format on
        },

        /**
         * Function Call
         */
        T(Compile) << T(llvmir::Ident)[Result] *
              (T(Expr) << T(Type) * T(FunCall)[FunCall]) >>
          [](Match& _) -> Node {
          Node funcall = _(FunCall);
          Node fun = funcall / Fun;
          Node param = funcall / Param;

          Node argIdent = llvmir::Ident ^ param->fresh();
          Node funIdent = llvmir::Ident ^ fun->fresh();

          return Seq << (Compile << argIdent->clone() << param)
                     << (Compile << funIdent->clone() << fun)
                     << (llvmir::Instr
                         << (llvmir::MiscOp
                             << (llvmir::Call
                                 << _(Result) << funIdent
                                 << (llvmir::ArgList << argIdent))));
        },

        /**
         * Closure Call
         */
        T(Compile) << T(llvmir::Ident)[Result] *
              (T(Expr) << (T(Type)) * T(ClosureCall)[ClosureCall]) >>
          [context](Match& _) -> Node {
          Node closCall = _(ClosureCall);
          Node fun = closCall / Fun;
          Node param = closCall / Param;
          Node typeArrow = get_type(fun);
          if (typeArrow != TypeArrow) {
            return err(typeArrow, "closure call type is not a function type");
          }
          // The type the caller sees for this specific call (after HM
          // instantiation), used to know each value's natural/unboxed type.
          Node funArgType = typeArrow / Ty1;
          Node funRetType = typeArrow / Ty2;

          // The callee's true declared type, which may still be a bare
          // TVar - that's masked by instantiation above whenever the
          // callee is a polymorphic `let`-bound global. A local
          // Param/FunDef-bound closure is never generalized, so
          // `typeArrow` is already its true declared type in that case.
          // NOTE: this only looks through a direct Global reference; a
          // partially-applied polymorphic closure flowing through an
          // intermediate, non-Global expression is not resolved (not
          // required for the currently supported polymorphism forms).
          Node declaredArrow = typeArrow;
          Node calleeExpr = fun / Expr;
          if (calleeExpr == Global) {
            auto it = context->globalTypes.find(node_val(calleeExpr));
            if (it != context->globalTypes.end() && it->second == TypeArrow) {
              declaredArrow = it->second;
            }
          }
          Node declaredArgType = declaredArrow / Ty1;
          Node declaredRetType = declaredArrow / Ty2;

          // The callee's actual physical LLVM signature.
          Node argLLVMType = getLLVMType(declaredArgType);
          Node retLLVMType = getLLVMType(declaredRetType);

          bool boxArg = declaredArgType == TVar && funArgType != TVar;
          bool unboxRet = declaredRetType == TVar && funRetType != TVar;

          std::string uniqueId = std::string(fun->fresh().view());
          Node closurePtr = llvmir::Ident ^ "closPtr_" + uniqueId;
          Node closureTy = llvmir::Ident ^ "ClosureTy";
          Node funSlotPtr = llvmir::Ident ^ "funSlot_" + uniqueId;
          Node funPtr = llvmir::Ident ^ "funPtr_" + uniqueId;
          Node funTy = llvmir::Ident ^ "funType_" + uniqueId;
          Node envSlotPtr = llvmir::Ident ^ "envSlot_" + uniqueId;
          Node envPtr = llvmir::Ident ^ "envPtr_" + uniqueId;
          Node argId = llvmir::Ident ^ param->fresh();
          Node funId = llvmir::Ident ^ param->fresh();

          // clang-format off
          Node GEPEnvSlotInClosure =
            (llvmir::Instr
             << (llvmir::MemoryOp
                 << (llvmir::GetElementPtr
                     << envSlotPtr
                     << closureTy
                     << closurePtr->clone()
                     << (llvmir::OffsetList
                         << (llvmir::Offset << llvmir::Ti32 << (llvmir::IRValue ^ "0"))
                         << (llvmir::Offset << llvmir::Ti32 << (llvmir::IRValue ^ "0"))))));
          Node loadEnvPtr =
            (llvmir::Instr
             << (llvmir::MemoryOp
                 << (llvmir::Load << envPtr << llvmir::TPtr << envSlotPtr->clone())));
          Node GEPFunSlotInClosure =
            (llvmir::Instr
             << (llvmir::MemoryOp
                 << (llvmir::GetElementPtr
                     << funSlotPtr
                     << closureTy->clone()
                     << closurePtr->clone()
                     << (llvmir::OffsetList
                         << (llvmir::Offset << llvmir::Ti32 << (llvmir::IRValue ^ "0"))
                         << (llvmir::Offset << llvmir::Ti32 << (llvmir::IRValue ^ "1"))))));
          Node loadFunPtr =
            (llvmir::Instr
             << (llvmir::MemoryOp
                 << (llvmir::Load << funPtr << llvmir::TPtr << funSlotPtr->clone())));
          Node createFunType =
            (llvmir::Action
             << (llvmir::CreateFunType
                 << funTy
                 << retLLVMType
                 << (llvmir::TypeList << llvmir::TPtr << llvmir::TPtr << argLLVMType)));

          // If the callee's declared parameter is boxed but this call site
          // has a concrete argument, convert it to the boxed word first.
          Node callArgId = argId;
          Node boxedArgId = nullptr;
          if (boxArg) {
            boxedArgId = llvmir::Ident ^ param->fresh();
            callArgId = boxedArgId;
          }

          // If the callee's declared return is boxed but this call site's
          // result is expected to be concrete, call into a raw temporary
          // and unbox it into `_(Result)`.
          Node callResultId = _(Result);
          if (unboxRet) {
            callResultId = llvmir::Ident ^ fun->fresh();
          }

          Node callFun =
            (llvmir::Instr
             << (llvmir::MiscOp
                 << (llvmir::CallOpaque
                     << callResultId << funTy->clone() << funPtr->clone()
                     << (llvmir::ArgList
                         << closurePtr->clone()
                         << envPtr->clone()
                         << callArgId->clone()))));

          Node result = Seq
            << (Compile << closurePtr << fun)
            << GEPEnvSlotInClosure
            << loadEnvPtr
            << GEPFunSlotInClosure
            << loadFunPtr
            << (Compile << argId << param);
          if (boxArg) {
            result
              << box_value(argId->clone(), boxedArgId, getLLVMType(funArgType));
          }
          result << createFunType << callFun;
          if (unboxRet) {
            result
              << unbox_value(
                   callResultId->clone(), _(Result), getLLVMType(funRetType));
          }
          return result;
          // clang-format on
        },

        /**
         * Print
         */
        T(Compile) << T(llvmir::Ident)[Result] *
              (T(Expr) << (T(Type) << T(TypeArrow)[TypeArrow]) * T(Print)) >>
          [](Match& _) -> Node {
          Node funcName = NULL;
          if ((_(TypeArrow) / Ty1)->type() != (_(TypeArrow) / Ty2)->type()) {
            return err(_(TypeArrow), "print is an identity function");
          }

          if ((_(TypeArrow) / Ty1 == TInt) && (_(TypeArrow) / Ty2 == TInt)) {
            funcName = llvmir::Ident ^ "native$printInt";
          } else if (
            (_(TypeArrow) / Ty1 == TBool) && (_(TypeArrow) / Ty2 == TBool)) {
            funcName = llvmir::Ident ^ "native$printBool";
          }

          if (!funcName) {
            return err(_(TypeArrow), "print function with this type not found");
          }

          return (
            llvmir::Action << (llvmir::GetFunction << _(Result) << funcName));
        },

        /**
         * Function Declaration
         */
        T(Compile) << T(IRFun)[IRFun] >> [](Match& _) -> Node {
          Node fun = _(IRFun);
          Node paramList = fun / ParamList;
          Node freeVarList = fun / FreeVarList;
          Node body = fun / Body;

          // TODO: Support polymorphism (Tvar)
          Node type = get_type(fun);
          if (type != TypeArrow) {
            return err(type, "function type is not a function type");
          }
          Node paramType = getLLVMType(type / Ty1);
          Node returnType = getLLVMType(type / Ty2);

          Node funType = llvmir::TypeArrow << paramType << returnType;

          std::string uniqueId = std::string(fun->fresh().view());
          Node entryPoint = llvmir::Label ^ ("entry_" + uniqueId);
          Node returnId = llvmir::Ident ^ ("ret_" + uniqueId);

          Node llvmFun = llvmir::IRFun ^ node_val(fun);
          Node returnInstr =
            (llvmir::TerminatorOp << (llvmir::Ret << returnId->clone()));

          if ("main" == node_val(fun)) {
            // clang-format off
            return Seq
              << (llvmFun
                  << funType
                  << (Compile << paramList)
                  << (llvmir::Body
                      << entryPoint
                      // main() may contain multiple expressions.
                      << (Compile << (PropagateCompile << *body))
                      // main() expected to return 0 for program success.
                      << (llvmir::Action
                          << (llvmir::CreateConst
                              << returnId->clone()
                              << llvmir::Ti32
                              << (llvmir::IRValue ^ "0")))
                      << returnInstr));
            // clang-format on
          } else {
            Node env = fun / Env;

            Node envType = llvmir::Ident ^ node_val(env);
            if (paramList->size() < 2) {
              return err(paramList, "too few parameters to function");
            }
            // A function body is a single (possibly let-nested) expression:
            // unlike main's body, it isn't a sequence of top-level
            // statements, so exactly one child is expected here.
            // Not prevented by WF since Body is shared with main functions,
            // that allow multiple expressions.
            if (body->size() != 1) {
              return err(
                body, "function body must contain exactly one expression");
            }
            Node envPtr = llvmir::Ident ^ node_val((paramList->at(1) / Ident));
            Node loadFreeVarsFromEnv =
              (Compile << envType->clone() << envPtr->clone() << freeVarList
                       << llvmir::Load);

            // clang-format off
            return Seq
              << (llvmFun
                  << funType
                  << (Compile << paramList)
                  << (llvmir::Body
                      << entryPoint
                      << loadFreeVarsFromEnv
                      << (Compile << returnId << *body)
                      // MiniML has implicit return so must insert it here.
                      << returnInstr));
            // clang-format on
          }
        },

        /**
         * Environment
         */
        T(Compile) << T(Env)[Env] >> [](Match& _) -> Node {
          Node env = _(Env);

          for (size_t i = 0, n = env->size(); i < n; i++) {
            Node type = env->at(i);
            env->replace_at(i, (Compile << type));
          }

          Node name = llvmir::Ident ^ node_val(env);
          auto typesToCompile = *env;

          return llvmir::Action
            << (llvmir::CreateStructType
                << name << (llvmir::TypeList << typesToCompile));
        },

        /**
         * Create Closure
         */
        T(Compile)
            << (T(llvmir::Ident)[Result] *
                (T(Expr) << (T(Type) * (T(CreateClosure)[CreateClosure])))) >>
          [](Match& _) -> Node {
          Node createClosure = _(CreateClosure);
          Node fun = createClosure / Fun;
          Node irFun = llvmir::Ident ^ node_val(fun);
          Node freeVarList = createClosure / FreeVarList;

          Node allocatorFun = llvmir::Ident ^ "malloc";

          Node closureBytes = llvmir::Ident ^ createClosure->fresh();
          Node closurePtr = _(Result);
          Node closureTy = llvmir::Ident ^ "ClosureTy";

          std::string uniqueId = std::string(createClosure->fresh().view());

          Node envSlotPtr = llvmir::Ident ^ "envSlot_" + uniqueId;
          Node envPtr = llvmir::Ident ^ "envPtr_" + uniqueId;

          Node funSlotPtr = llvmir::Ident ^ "funSlot_" + uniqueId;
          Node funPtr = llvmir::Ident ^ "funPtr_" + uniqueId;

          // clang-format off
          Node getClosureSize =
            (llvmir::Action
             << (llvmir::GetSizeOfType
                 << closureBytes << llvmir::Ti64 << closureTy));
          Node allocateClosure =
            (llvmir::Instr
             << (llvmir::MiscOp
                 << (llvmir::Call
                     << closurePtr
                     << allocatorFun->clone()
                     << (llvmir::ArgList << closureBytes->clone()))));
          Node GEPFunSlotInClosure =
            (llvmir::Instr
             << (llvmir::MemoryOp
                 << (llvmir::GetElementPtr
                     << funSlotPtr
                     << closureTy->clone()
                     << closurePtr->clone()
                     << (llvmir::OffsetList
                         << (llvmir::Offset << llvmir::Ti32 << (llvmir::IRValue ^ "0"))
                         << (llvmir::Offset << llvmir::Ti32 << (llvmir::IRValue ^ "1"))))));
          Node getFunPtr =
            (llvmir::Action
             << (llvmir::GetFunction << funPtr << irFun));
          Node storeFunPtrInClosure =
            (llvmir::Instr
             << (llvmir::MemoryOp
                 << (llvmir::Store << funPtr->clone() << funSlotPtr->clone())));
          // clang-format on

          if (freeVarList->size() > 0) {
            Node env = createClosure / Env;

            Node envBytes = llvmir::Ident ^ createClosure->fresh();
            Node envTy = llvmir::Ident ^ node_val(env);

            // clang-format off
            Node getEnvSize =
              (llvmir::Action
               << (llvmir::GetSizeOfType
                   << envBytes << llvmir::Ti64 << envTy));
            Node allocateEnv =
              (llvmir::Instr
               << (llvmir::MiscOp
                   << (llvmir::Call
                       << envPtr
                       << allocatorFun
                       << (llvmir::ArgList << envBytes->clone()))));
            Node storeFreeVarsInEnv =
              (Compile
               << envTy->clone() << envPtr->clone() << freeVarList << llvmir::Store);
            Node GEPEnvSlotInClosure =
              (llvmir::Instr
               << (llvmir::MemoryOp
                   << (llvmir::GetElementPtr
                       << envSlotPtr
                       << closureTy->clone()
                       << closurePtr->clone()
                       << (llvmir::OffsetList
                           << (llvmir::Offset << llvmir::Ti32 << (llvmir::IRValue ^ "0"))
                           << (llvmir::Offset << llvmir::Ti32 << (llvmir::IRValue ^ "0"))))));
            Node storeEnvPtrInClosure =
              (llvmir::Instr
               << (llvmir::MemoryOp
                   << (llvmir::Store << envPtr->clone() << envSlotPtr->clone())));

            return Seq << getClosureSize
                       << allocateClosure
                       << getEnvSize
                       << allocateEnv
                       << storeFreeVarsInEnv
                       << GEPEnvSlotInClosure
                       << storeEnvPtrInClosure
                       << GEPFunSlotInClosure
                       << getFunPtr
                       << storeFunPtrInClosure;
          } else {
            return Seq << getClosureSize
                       << allocateClosure
                       << GEPFunSlotInClosure
                       << getFunPtr
                       << storeFunPtrInClosure;
          }
          // clang-format on
        },

        /**
         * Type
         */
        T(Compile) << T(Type)[Type] >> [](Match& _) -> Node {
          Node type = _(Type) / Type;
          Node llvmType = getLLVMType(type);
          if (llvmType == nullptr) {
            return err(type, "Cannot convert to equivalent LLVM IR type");
          }

          return llvmType;
        },

        /**
         * ParamList
         */
        T(Compile) << T(ParamList)[ParamList] >> [](Match& _) -> Node {
          Node paramList = _(ParamList);

          for (size_t i = 0, n = paramList->size(); i < n; i++) {
            Node param = paramList->at(i);
            paramList->replace_at(i, (Compile << param));
          }

          return (llvmir::ParamList << *paramList);
        },

        /**
         * Parameter
         */
        T(Compile) << T(Param)[Param] >> [](Match& _) -> Node {
          Node param = _(Param);
          Node ident = param / Ident;
          Node irIdent = llvmir::Ident ^ node_val(ident);

          Node type = get_type(param);
          Node llvmType = getLLVMType(type);

          return llvmir::Param << irIdent << llvmType;
        },

        /**
         * FreeVarList (Populate environment)
         */
        T(Compile) << T(llvmir::Ident)[Env] * T(llvmir::Ident)[TPtr] *
              T(FreeVarList)[FreeVarList] * T(llvmir::Store) >>
          [](Match& _) -> Node {
          Node envPtr = _(TPtr);
          Node envTy = _(Env);
          Node freeVarList = _(FreeVarList);

          Node seq = Seq;
          for (size_t i = 0, n = freeVarList->size(); i < n; i++) {
            Node freeVar = freeVarList->at(i);
            Node ident = freeVar / Ident;
            Node irIdent = llvmir::Ident ^ node_val(ident);
            Node type = getLLVMType(get_type(freeVar));

            Node tmp = llvmir::Ident ^ freeVarList->fresh();
            Node freeVarSlot = llvmir::Ident ^ freeVarList->fresh();

            // clang-format off
            Node getFreeVar = nullptr;
            if (ident == Global) {
              Node loadInstr =
                (llvmir::Instr
                 << (llvmir::MemoryOp
                     << (llvmir::Load << tmp << type->clone() << irIdent)));

              getFreeVar = loadInstr;
            } else {
              Node bitcastInstr =
                (llvmir::Instr
                 << (llvmir::ConversionOp
                     << (llvmir::BitCast << tmp << irIdent->clone() << type->clone())));

              getFreeVar = bitcastInstr;
            }

            Node GEPFreeVarSlotInEnv =
              (llvmir::Instr
               << (llvmir::MemoryOp
                   << (llvmir::GetElementPtr
                       << freeVarSlot << envTy->clone() << envPtr->clone()
                       << (llvmir::OffsetList
                           << (llvmir::Offset << llvmir::Ti32
                                              << (llvmir::IRValue ^ "0"))
                           << (llvmir::Offset << llvmir::Ti32
                                              << (llvmir::IRValue ^ std::to_string(i)))))));
            Node storeFreeVarInEnv =
              (llvmir::Instr
               << (llvmir::MemoryOp
                   << (llvmir::Store << tmp->clone() << freeVarSlot->clone())));

            seq << getFreeVar
                << GEPFreeVarSlotInEnv
                << storeFreeVarInEnv;
            // clang-format on
          }

          return seq;
        },

        /**
         * FreeVarList (Environment -> Registers)
         */
        T(Compile) << T(llvmir::Ident)[Env] * T(llvmir::Ident)[TPtr] *
              T(FreeVarList)[FreeVarList] * T(llvmir::Load) >>
          [](Match& _) -> Node {
          Node envPtr = _(TPtr);
          Node envTy = _(Env);
          Node freeVarList = _(FreeVarList);

          Node seq = Seq;
          for (size_t i = 0, n = freeVarList->size(); i < n; i++) {
            Node freeVar = freeVarList->at(i);
            Node ident = freeVar / Ident;
            Node irIdent = llvmir::Ident ^ node_val(ident);
            Node type = getLLVMType(get_type(freeVar));

            Node tmp = llvmir::Ident ^ freeVarList->fresh();
            Node freeVarSlot = llvmir::Ident ^ freeVarList->fresh();

            // clang-format off
            Node GEPFreeVarSlotInEnv =
              (llvmir::Instr
               << (llvmir::MemoryOp
                   << (llvmir::GetElementPtr
                       << freeVarSlot->clone()
                       << envTy->clone()
                       << envPtr->clone()
                       << (llvmir::OffsetList
                           << (llvmir::Offset << llvmir::Ti32
                                              << (llvmir::IRValue ^ "0"))
                           << (llvmir::Offset << llvmir::Ti32
                                              << (llvmir::IRValue ^ std::to_string(i)))))));
            Node loadFreeVarFromEnv =
              (llvmir::Instr
               << (llvmir::MemoryOp
                   << (llvmir::Load << tmp->clone() << type->clone() << freeVarSlot->clone())));
            Node allocaFreeVar =
              (llvmir::Instr
               << (llvmir::MemoryOp
                   << (llvmir::Alloca << irIdent->clone() << type->clone())));
            Node storeFreeVarOnStack =
              (llvmir::Instr
               << (llvmir::MemoryOp
                   << (llvmir::Store << tmp->clone() << irIdent->clone())));

            seq << GEPFreeVarSlotInEnv
                << loadFreeVarFromEnv
                << allocaFreeVar
                << storeFreeVarOnStack;
            // clang-format on
          }

          return seq;
        },

        // Propagate Compile onto several nodes.
        T(Compile) << T(PropagateCompile)[PropagateCompile] >>
          [](Match& _) -> Node {
          for (size_t i = 0, n = _(PropagateCompile)->size(); i < n; i++) {
            Node child = _(PropagateCompile)->at(i);
            Node ident = llvmir::Ident ^ _(PropagateCompile)->fresh();
            _(PropagateCompile)->replace_at(i, Compile << ident << child);
          }

          auto children = *_(PropagateCompile);

          return Seq << children;
        },

        T(Compile) << T(llvmir::Ident) * T(Expr)[Expr] >> [](Match& _) -> Node {
          return err(_(Expr), "Cannot compile ill-typed expression");
        },
      }};
  }
}
