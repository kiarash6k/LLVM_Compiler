#ifndef COMP_PARSER_PARSER_H
#define COMP_PARSER_PARSER_H

#include "comp/Basic/Diagnostic.h"
#include "comp/Lexer/Lexer.h"
#include "comp/Sema/Sema.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/raw_ostream.h"

namespace comp {

class Parser {

  Lexer &Lex;

  Sema &Actions;

  Token Tok;

  DiagnosticsEngine &getDiagnostics() const {
    return Lex.getDiagnostics();
  }

  void advance() { Lex.next(Tok); }

  bool expect(tok::TokenKind ExpectedTok) {
    if (Tok.is(ExpectedTok)) {
      return false;
    }
    // There must be a better way!
    const char *Expected =
        tok::getPunctuatorSpelling(ExpectedTok);
    if (!Expected)
      Expected = tok::getKeywordSpelling(ExpectedTok);
    llvm::StringRef Actual(Tok.getLocation().getPointer(),
                           Tok.getLength());
    getDiagnostics().report(Tok.getLocation(),
                            diag::err_expected, Expected,
                            Actual);
    return true;
  }

  bool consume(tok::TokenKind ExpectedTok) {
    if (Tok.is(ExpectedTok)) {
      advance();
      return false;
    }
    return true;
  }

  bool parsePower(Expr *&E);
  bool parseImport();
  bool parseCompilationUnit(ModuleDeclaration *&D);
  bool parseBlock(DeclList &Decls, StmtList &Stmts);
  bool parseDeclaration(DeclList &Decls);
  bool parseVariableDeclaration(DeclList &Decls);
  bool parseFormalParameterList(FormalParamList &Params);
  bool parseFormalParameter(FormalParamList &Params);
  bool parseStatementSequence(StmtList &Stmts);
  bool parseStatement(StmtList &Stmts);
  bool parseIfStatement(StmtList &Stmts);
  bool parseLoopStatement(StmtList &Stmts);
  bool parseExpList(ExprList &Exprs);
  bool parseExpression(Expr *&E);
  bool parseRelation(OperatorInfo &Op);
  bool parseSimpleExpression(Expr *&E);
  bool parseAddOperator(OperatorInfo &Op);
  bool parseTerm(Expr *&E);
  bool parseMulOperator(OperatorInfo &Op);
  bool parseFactor(Expr *&E);
  bool parseEqualOps(Decl *&D);
  bool parseIdentList(IdentList &Ids);

public:
  Parser(Lexer &Lex, Sema &Actions);

  ModuleDeclaration *parse();
};
} // namespace comp
#endif