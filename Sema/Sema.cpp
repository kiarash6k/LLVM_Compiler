#include "comp/Sema/Sema.h"
#include "llvm/Support/raw_ostream.h"

using namespace comp;

void Sema::enterScope(Decl *D) {
  CurrentScope = new Scope(CurrentScope);
  CurrentDecl = D;
}

void Sema::leaveScope() {
  assert(CurrentScope && "Can't leave non-existing scope");
  Scope *Parent = CurrentScope->getParent();
  delete CurrentScope;
  CurrentScope = Parent;
  CurrentDecl = CurrentDecl->getEnclosingDecl();
}

bool Sema::isOperatorForType(tok::TokenKind Op,
                             TypeDeclaration *Ty) {
  switch (Op) {
  case tok::plus:
  case tok::minus:
  case tok::star:
  case tok::mod:
    return Ty == IntegerType;
  case tok::slash:
    return false; // REAL not implemented
  case tok::kw_and:
  case tok::kw_or:
    return Ty == BooleanType;
  default:
    llvm_unreachable("Unknown operator");
  }
}

void Sema::initialize() {
  // Setup global scope.
  CurrentScope = new Scope();
  CurrentDecl = nullptr;
  IntegerType =
      new TypeDeclaration(CurrentDecl, SMLoc(), "INTEGER");
  BooleanType =
      new TypeDeclaration(CurrentDecl, SMLoc(), "BOOLEAN");
  TrueLiteral = new BooleanLiteral(true, BooleanType);
  FalseLiteral = new BooleanLiteral(false, BooleanType);
  TrueConst = new ConstantDeclaration(CurrentDecl, SMLoc(),
                                      "TRUE", TrueLiteral);
  FalseConst = new ConstantDeclaration(
      CurrentDecl, SMLoc(), "FALSE", FalseLiteral);
  CurrentScope->insert(IntegerType);
  CurrentScope->insert(BooleanType);
  CurrentScope->insert(TrueConst);
  CurrentScope->insert(FalseConst);
}

void Sema::actOnImport(StringRef ModuleName,
                       IdentList &Ids) {
  Diags.report(SMLoc(), diag::err_not_yet_implemented);
}

void Sema::actOnConstantDeclaration(DeclList &Decls,
                                    SMLoc Loc,
                                    StringRef Name,
                                    Expr *E) {
  assert(CurrentScope && "CurrentScope not set");
  ConstantDeclaration *Decl =
      new ConstantDeclaration(CurrentDecl, Loc, Name, E);
  if (CurrentScope->insert(Decl))
    Decls.push_back(Decl);
  else
    Diags.report(Loc, diag::err_symbold_declared, Name);
}

void Sema::actOnVariableDeclaration(DeclList &Decls,
                                    IdentList &Ids,
                                    Decl *D) {
  assert(CurrentScope && "CurrentScope not set");
  if (TypeDeclaration *Ty = dyn_cast<TypeDeclaration>(D)) {
    for (auto I = Ids.begin(), E = Ids.end(); I != E; ++I) {
      SMLoc Loc = I->first;
      StringRef Name = I->second;
      VariableDeclaration *Decl = new VariableDeclaration(
          CurrentDecl, Loc, Name, Ty);
      if (CurrentScope->insert(Decl))
        Decls.push_back(Decl);
      else
        Diags.report(Loc, diag::err_symbold_declared, Name);
    }
  } else if (!Ids.empty()) {
    SMLoc Loc = Ids.front().first;
    Diags.report(Loc, diag::err_vardecl_requires_type);
  }
}

void Sema::actOnAssignment(StmtList &Stmts, SMLoc Loc,
                           Decl *D, Expr *E) {
  if (auto Var = dyn_cast<VariableDeclaration>(D)) {
    if (Var->getType() != E->getType()) {}
    Stmts.push_back(new AssignmentStatement(Var, E));
  } else if (D) {
    // TODO Emit error
  }
}

void Sema::actOnIfStatement(StmtList &Stmts, SMLoc Loc,
                            Expr *Cond, StmtList &IfStmts,
                            StmtList &ElseStmts) {
  if (!Cond)
    Cond = FalseLiteral;

  if (Cond->getType() != BooleanType) {
    Diags.report(Loc, diag::err_if_expr_must_be_bool);
  }
  Stmts.push_back(
      new IfStatement(Cond, IfStmts, ElseStmts));
}

void Sema::actOnLoopStatement(StmtList &Stmts, SMLoc Loc,
                               Expr *Cond,
                               StmtList &LoopStmts) {
  if (!Cond)
    Cond = FalseLiteral;

  if (Cond->getType() != BooleanType) {
    Diags.report(Loc, diag::err_Loop_expr_must_be_bool);
  }
  Stmts.push_back(new LoopStatement(Cond, LoopStmts));
}

Expr *Sema::actOnExpression(Expr *Left, Expr *Right,
                            const OperatorInfo &Op) {
  // Relation
  if (!Left)
    return Right;
  if (!Right)
    return Left;

  if (Left->getType() != Right->getType()) {
    Diags.report(
        Op.getLocation(),
        diag::err_types_for_operator_not_compatible,
        tok::getPunctuatorSpelling(Op.getKind()));
  }
  bool IsConst = Left->isConst() && Right->isConst();
  return new InfixExpression(Left, Right, Op, BooleanType,
                             IsConst);
}

Expr *Sema::actOnSimpleExpression(Expr *Left, Expr *Right,
                                  const OperatorInfo &Op) {
  // Addition
  if (!Left)
    return Right;
  if (!Right)
    return Left;

  if (Left->getType() != Right->getType()) {
    Diags.report(
        Op.getLocation(),
        diag::err_types_for_operator_not_compatible,
        tok::getPunctuatorSpelling(Op.getKind()));
  }
  TypeDeclaration *Ty = Left->getType();
  bool IsConst = Left->isConst() && Right->isConst();
  if (IsConst && Op.getKind() == tok::kw_or) {
    BooleanLiteral *L = dyn_cast<BooleanLiteral>(Left);
    BooleanLiteral *R = dyn_cast<BooleanLiteral>(Right);
    return L->getValue() || R->getValue() ? TrueLiteral
                                          : FalseLiteral;
  }
  return new InfixExpression(Left, Right, Op, Ty, IsConst);
}

Expr *Sema::actOnTerm(Expr *Left, Expr *Right,
                      const OperatorInfo &Op) {
  // Multiplication
  if (!Left)
    return Right;
  if (!Right)
    return Left;

  if (Left->getType() != Right->getType() ||
      !isOperatorForType(Op.getKind(), Left->getType())) {
    Diags.report(
        Op.getLocation(),
        diag::err_types_for_operator_not_compatible,
        tok::getPunctuatorSpelling(Op.getKind()));
  }
  TypeDeclaration *Ty = Left->getType();
  bool IsConst = Left->isConst() && Right->isConst();
  if (IsConst && Op.getKind() == tok::kw_and) {
    BooleanLiteral *L = dyn_cast<BooleanLiteral>(Left);
    BooleanLiteral *R = dyn_cast<BooleanLiteral>(Right);
    return L->getValue() && R->getValue() ? TrueLiteral
                                          : FalseLiteral;
  }
  return new InfixExpression(Left, Right, Op, Ty, IsConst);
}

Expr *Sema::actOnPrefixExpression(Expr *E,
                                  const OperatorInfo &Op) {
  if (!E)
    return nullptr;

  if (!isOperatorForType(Op.getKind(), E->getType())) {
    Diags.report(
        Op.getLocation(),
        diag::err_types_for_operator_not_compatible,
        tok::getPunctuatorSpelling(Op.getKind()));
  }

  if (Op.getKind() == tok::minus) {
    bool Ambiguous = true;
    if (isa<IntegerLiteral>(E) || isa<VariableAccess>(E) ||
        isa<ConstantAccess>(E))
      Ambiguous = false;
    else if (auto *Infix = dyn_cast<InfixExpression>(E)) {
      tok::TokenKind Kind =
          Infix->getOperatorInfo().getKind();
      if (Kind == tok::star || Kind == tok::slash)
        Ambiguous = false;
    }
    if (Ambiguous) {
      Diags.report(Op.getLocation(),
                   diag::warn_ambigous_negation);
    }
  }

  return new PrefixExpression(E, Op, E->getType(),
                              E->isConst());
}

Expr *Sema::actOnIntegerLiteral(SMLoc Loc,
                                StringRef Literal) {
  uint8_t Radix = 10;
  if (Literal.endswith("H")) {
    Literal = Literal.drop_back();
    Radix = 16;
  }
  llvm::APInt Value(64, Literal, Radix);
  return new IntegerLiteral(Loc, llvm::APSInt(Value, false),
                            IntegerType);
}

Expr *Sema::actOnVariable(Decl *D) {
  if (!D)
    return nullptr;
  if (auto *V = dyn_cast<VariableDeclaration>(D))
    return new VariableAccess(V);
  else if (auto *P = dyn_cast<FormalParameterDeclaration>(D))
    return new VariableAccess(P);
  else if (auto *C = dyn_cast<ConstantDeclaration>(D)) {
    if (C == TrueConst)
      return TrueLiteral;
    if (C == FalseConst) {
      return FalseLiteral;
    }
    return new ConstantAccess(C);
  }
  return nullptr;
}

Decl *Sema::actOnQualIdentPart(Decl *Prev, SMLoc Loc,
                               StringRef Name) {
  if (!Prev) {
    if (Decl *D = CurrentScope->lookup(Name))
      return D;
  } else if (auto *Mod =
                 dyn_cast<ModuleDeclaration>(Prev)) {
    auto Decls = Mod->getDecls();
    for (auto I = Decls.begin(), E = Decls.end(); I != E;
         ++I) {
      if ((*I)->getName() == Name) {
        return *I;
      }
    }
  } else {
    llvm_unreachable("Semantic Error");
  }
  Diags.report(Loc, diag::err_undeclared_name, Name);
  return nullptr;
}