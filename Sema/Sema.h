#ifndef COMP_SEMA_SEMA_H
#define COMP_SEMA_SEMA_H

#include "comp/AST/AST.h"
#include "comp/Basic/Diagnostic.h"
#include "comp/Sema/Scope.h"
#include <memory>

namespace comp {

class Sema {
  friend class EnterDeclScope;
  void enterScope(Decl *);
  void leaveScope();

  bool isOperatorForType(tok::TokenKind Op,
                         TypeDeclaration *Ty);

  void checkFormalAndActualParameters(
      SMLoc Loc, const FormalParamList &Formals,
      const ExprList &Actuals);

  Scope *CurrentScope;
  Decl *CurrentDecl;
  DiagnosticsEngine &Diags;

  TypeDeclaration *IntegerType;
  TypeDeclaration *BooleanType;
  BooleanLiteral *TrueLiteral;
  BooleanLiteral *FalseLiteral;
  ConstantDeclaration *TrueConst;
  ConstantDeclaration *FalseConst;

public:
  Sema(DiagnosticsEngine &Diags)
      : CurrentScope(nullptr), CurrentDecl(nullptr),
        Diags(Diags) {
    initialize();
  }

  void initialize();

  ModuleDeclaration *actOnModuleDeclaration(SMLoc Loc,
                                            StringRef Name);
  void actOnImport(StringRef ModuleName, IdentList &Ids);
  void actOnConstantDeclaration(DeclList &Decls, SMLoc Loc,
                                StringRef Name, Expr *E);
  void actOnVariableDeclaration(DeclList &Decls,
                                IdentList &Ids, Decl *D);
  void
  actOnFormalParameterDeclaration(FormalParamList &Params,
                                  IdentList &Ids, Decl *D,
                                  bool IsVar);
  void actOnAssignment(StmtList &Stmts, SMLoc Loc, Decl *D,
                       Expr *E);
  void actOnIfStatement(StmtList &Stmts, SMLoc Loc,
                        Expr *Cond, StmtList &IfStmts,
                        StmtList &ElseStmts);
  void actOnLoopStatement(StmtList &Stmts, SMLoc Loc,
                           Expr *Cond,
                           StmtList &WhileStmts);

  Expr *actOnExpression(Expr *Left, Expr *Right,
                        const OperatorInfo &Op);
  Expr *actOnSimpleExpression(Expr *Left, Expr *Right,
                              const OperatorInfo &Op);
  Expr *actOnTerm(Expr *Left, Expr *Right,
                  const OperatorInfo &Op);
  Expr *actOnPrefixExpression(Expr *E,
                              const OperatorInfo &Op);
  Expr *actOnIntegerLiteral(SMLoc Loc, StringRef Literal);
  Expr *actOnVariable(Decl *D);
  Decl *actOnQualIdentPart(Decl *Prev, SMLoc Loc,
                           StringRef Name);
};

class EnterDeclScope {
  Sema &Semantics;

public:
  EnterDeclScope(Sema &Semantics, Decl *D)
      : Semantics(Semantics) {
    Semantics.enterScope(D);
  }
  ~EnterDeclScope() { Semantics.leaveScope(); }
};
} // namespace comp
#endif