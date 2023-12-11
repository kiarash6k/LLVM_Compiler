#include "tinylang/Parser/Parser.h"
#include "tinylang/Basic/TokenKinds.h"

using namespace tinylang;

namespace {
OperatorInfo fromTok(Token Tok) {
  return OperatorInfo(Tok.getLocation(), Tok.getKind());
}
} // namespace

Parser::Parser(Lexer &Lex, Sema &Actions)
    : Lex(Lex), Actions(Actions) {
  advance();
}

bool Parser::parsePower(Expr *&E) {
  {
    if (parseFactor(E))
      goto _error;
    while (Tok.is(tok::power)) {
      OperatorInfo Op;
      Expr *Right = nullptr;
      if (Tok.is(tok::power))
        goto _error;
      if (parseFactor(Right))
        goto _error;
    }
    return false;
  }
_error:
  while (!Tok.isOneOf(tok::r_paren, tok::plus,
                      tok::comma, tok::minus, tok::semi,
                      tok::less, tok::lessequal, tok::equal,
                      tok::greater, tok::greaterequal,
                      tok::kw_else, tok::kw_end,
                      tok::kw_or, tok::kw_begin)) {
    advance();
    if (Tok.is(tok::eof))
      return true;
  }
  return false;
}

bool Parser::parseImport() {
  {
    IdentList Ids;
    StringRef ModuleName;
    if (Tok.is(tok::kw_FROM)) {
      advance();
      if (expect(tok::identifier))
        goto _error;
      ModuleName = Tok.getIdentifier();
      advance();
    }
    if (consume(tok::kw_IMPORT))
      goto _error;
    if (parseIdentList(Ids))
      goto _error;
    if (expect(tok::semi))
      goto _error;
    Actions.actOnImport(ModuleName, Ids);
    advance();
    return false;
  }
_error:
  while (!Tok.isOneOf(tok::kw_begin,tok::kw_end,
                      tok::kw_FROM,tok::kw_IMPORT,
                      tok::kw_int)) {
    advance();
    if (Tok.is(tok::eof))
      return true;
  }
  return false;
}

bool Parser::parseBlock(DeclList &Decls, StmtList &Stmts) {
  {
    while (Tok.is(tok::kw_int)) {
      if (parseDeclaration(Decls))
        goto _error;
    }
    if (Tok.is(tok::kw_begin)) {
      advance();
      if (parseStatementSequence(Stmts))
        goto _error;
    }
    if (consume(tok::kw_end))
      goto _error;
    return false;
  }
_error:
  while (!Tok.is(tok::identifier)) {
    advance();
    if (Tok.is(tok::eof))
      return true;
  }
  return false;
}

bool Parser::parseDeclaration(DeclList &Decls) {
  {
    if (Tok.is(tok::kw_int)) {
      advance();
      while (Tok.is(tok::identifier)) {
        if (parseVariableDeclaration(Decls))
          goto _error;
        if (consume(tok::semi))
          goto _error;
      }
    } else {
      /*ERROR*/
      goto _error;
    }
    return false;
  }
_error:
  while (!Tok.isOneOf(tok::kw_begin,
                      tok::kw_end,
                      tok::kw_int)) {
    advance();
    if (Tok.is(tok::eof))
      return true;
  }
  return false;
}

bool Parser::parseVariableDeclaration(DeclList &Decls) {
  {
    Decl *D;
    IdentList Ids;
    if (parseIdentList(Ids))
      goto _error;
    if (consume(tok::colon))
      goto _error;
    return false;
  }
_error:
  while (!Tok.is(tok::semi)) {
    advance();
    if (Tok.is(tok::eof))
      return true;
  }
  return false;
}

bool Parser::parseFormalParameterList(
    FormalParamList &Params) {
  {
    if (parseFormalParameter(Params))
      goto _error;
    while (Tok.is(tok::semi)) {
      advance();
      if (parseFormalParameter(Params))
        goto _error;
    }
    return false;
  }
_error:
  while (!Tok.is(tok::r_paren)) {
    advance();
    if (Tok.is(tok::eof))
      return true;
  }
  return false;
}

bool Parser::parseStatementSequence(StmtList &Stmts) {
  {
    if (parseStatement(Stmts))
      goto _error;
    while (Tok.is(tok::semi)) {
      advance();
      if (parseStatement(Stmts))
        goto _error;
    }
    return false;
  }
_error:
  while (!Tok.isOneOf(tok::kw_else, tok::kw_elif, tok::kw_end)) {
    advance();
    if (Tok.is(tok::eof))
      return true;
  }
  return false;
}

bool Parser::parseStatement(StmtList &Stmts) {
  {
    if (Tok.is(tok::identifier)) {
      Decl *D;
      Expr *E = nullptr;
      SMLoc Loc = Tok.getLocation();
      if (parseEqualOps(D))
        goto _error;
      if (Tok.is(tok::colonequal)) {
        advance();
        if (parseExpression(E))
          goto _error;
        Actions.actOnAssignment(Stmts, Loc, D, E);
      } else if (Tok.is(tok::l_paren)) {
        ExprList Exprs;
        if (Tok.is(tok::l_paren)) {
          advance();
          if (Tok.isOneOf(tok::l_paren, tok::plus,
                          tok::minus, tok::kw_NOT,
                          tok::identifier,
                          tok::integer_literal)) {
            if (parseExpList(Exprs))
              goto _error;
          }
          if (consume(tok::r_paren))
            goto _error;
        }
        Actions.actOnProcCall(Stmts, Loc, D, Exprs);
      }
    } else if (Tok.is(tok::kw_if)) {
      if (parseIfStatement(Stmts))
        goto _error;
    } else if (Tok.is(tok::kw_loop)) {
      if (parseLoopStatement(Stmts))
        goto _error;
    } else {
      /*ERROR*/
      goto _error;
    }
    return false;
  }
_error:
  while (
      !Tok.isOneOf(tok::semi, tok::kw_else, tok::kw_end)) {
    advance();
    if (Tok.is(tok::eof))
      return true;
  }
  return false;
}

bool Parser::parseIfStatement(StmtList &Stmts) {
  {
    Expr *E = nullptr;
    StmtList IfStmts, ElseStmts;
    SMLoc Loc = Tok.getLocation();
    if (consume(tok::kw_if))
      goto _error;
    if (parseExpression(E))
      goto _error;
    if (consume(tok::kw_begin))
      goto _error;
    if (parseStatementSequence(IfStmts))
      goto _error;
    if (Tok.is(tok::kw_elif)) {
      advance();
      if (parseStatementSequence(ElseStmts))
        goto _error;
    }
    if (Tok.is(tok::kw_else)) {
      advance();
      if (parseStatementSequence(ElseStmts))
        goto _error;
    }
    if (expect(tok::kw_end))
      goto _error;
    Actions.actOnIfStatement(Stmts, Loc, E, IfStmts,
                             ElseStmts);
    advance();
    return false;
  }
_error:
  while (
      !Tok.isOneOf(tok::semi, tok::kw_else, tok::kw_end)) {
    advance();
    if (Tok.is(tok::eof))
      return true;
  }
  return false;
}

bool Parser::parseLoopStatement(StmtList &Stmts) {
  {
    Expr *E = nullptr;
    StmtList LoopStmts;
    SMLoc Loc = Tok.getLocation();
    if (consume(tok::kw_loop))
      goto _error;
    if (parseExpression(E))
      goto _error;
    if (consume(tok::kw_DO))
      goto _error;
    if (parseStatementSequence(LoopStmts))
      goto _error;
    if (expect(tok::kw_end))
      goto _error;
    Actions.actOnWhileStatement(Stmts, Loc, E, LoopStmts);
    advance();
    return false;
  }
_error:
  while (
      !Tok.isOneOf(tok::semi, tok::kw_else, tok::kw_end)) {
    advance();
    if (Tok.is(tok::eof))
      return true;
  }
  return false;
}

bool Parser::parseExpList(ExprList &Exprs) {
  {
    Expr *E = nullptr;
    if (parseExpression(E))
      goto _error;
    if (E)
      Exprs.push_back(E);
    while (Tok.is(tok::comma)) {
      E = nullptr;
      advance();
      if (parseExpression(E))
        goto _error;
      if (E)
        Exprs.push_back(E);
    }
    return false;
  }
_error:
  while (!Tok.is(tok::r_paren)) {
    advance();
    if (Tok.is(tok::eof))
      return true;
  }
  return false;
}

bool Parser::parseExpression(Expr *&E) {
  {
    if (parseSimpleExpression(E))
      goto _error;
    if (Tok.isOneOf(tok::less, tok::lessequal,
                    tok::equal, tok::greater,
                    tok::greaterequal)) {
      OperatorInfo Op;
      Expr *Right = nullptr;
      if (parseRelation(Op))
        goto _error;
      if (parseSimpleExpression(Right))
        goto _error;
      E = Actions.actOnExpression(E, Right, Op);
    }
    return false;
  }
_error:
  while (!Tok.isOneOf(tok::r_paren, tok::comma, tok::semi,
                      tok::kw_begin, tok::kw_else, tok::kw_end,
                      tok::kw_begin)) {
    advance();
    if (Tok.is(tok::eof))
      return true;
  }
  return false;
}

bool Parser::parseRelation(OperatorInfo &Op) {
  {
    if (Tok.is(tok::equal)) {
      Op = fromTok(Tok);
      advance();
    } else if (Tok.is(tok::less)) {
      Op = fromTok(Tok);
      advance();
    } else if (Tok.is(tok::lessequal)) {
      Op = fromTok(Tok);
      advance();
    } else if (Tok.is(tok::greater)) {
      Op = fromTok(Tok);
      advance();
    } else if (Tok.is(tok::greaterequal)) {
      Op = fromTok(Tok);
      advance();
    } else {
      /*ERROR*/
      goto _error;
    }
    return false;
  }
_error:
  while (!Tok.isOneOf(tok::l_paren, tok::plus, tok::minus,
                      tok::identifier, tok::integer_literal)) {
    advance();
    if (Tok.is(tok::eof))
      return true;
  }
  return false;
}

bool Parser::parseSimpleExpression(Expr *&E) {
  {
    OperatorInfo PrefixOp;
    if (Tok.isOneOf(tok::plus, tok::minus)) {
      if (Tok.is(tok::plus)) {
        PrefixOp = fromTok(Tok);
        advance();
      } else if (Tok.is(tok::minus)) {
        PrefixOp = fromTok(Tok);
        advance();
      }
    }
    if (parseTerm(E))
      goto _error;
    while (Tok.isOneOf(tok::plus, tok::minus, tok::kw_or)) {
      OperatorInfo Op;
      Expr *Right = nullptr;
      if (parseAddOperator(Op))
        goto _error;
      if (parseTerm(Right))
        goto _error;
      E = Actions.actOnSimpleExpression(E, Right, Op);
    }
    if (!PrefixOp.isUnspecified())

      E = Actions.actOnPrefixExpression(E, PrefixOp);
    return false;
  }
_error:
  while (!Tok.isOneOf(
      tok::r_paren, tok::comma, tok::semi,
      tok::less, tok::lessequal, tok::equal, tok::greater,
      tok::greaterequal, tok::kw_else,
      tok::kw_end, tok::kw_begin)) {
    advance();
    if (Tok.is(tok::eof))
      return true;
  }
  return false;
}

bool Parser::parseAddOperator(OperatorInfo &Op) {
  {
    if (Tok.is(tok::plus)) {
      Op = fromTok(Tok);
      advance();
    } else if (Tok.is(tok::minus)) {
      Op = fromTok(Tok);
      advance();
    } else if (Tok.is(tok::kw_or)) {
      Op = fromTok(Tok);
      advance();
    } else {
      /*ERROR*/
      goto _error;
    }
    return false;
  }
_error:
  while (!Tok.isOneOf(tok::l_paren,
                      tok::identifier,
                      tok::integer_literal)) {
    advance();
    if (Tok.is(tok::eof))
      return true;
  }
  return false;
}

bool Parser::parseTerm(Expr *&E) {
  {
    if (parsePower(E))
      goto _error;
    while (Tok.isOneOf(tok::star, tok::slash, tok::kw_and,
                       tok::kw_div, tok::kw_mod)) {
      OperatorInfo Op;
      Expr *Right = nullptr;
      if (parseMulOperator(Op))
        goto _error;
      if (parsePower(Right))
        goto _error;
      E = Actions.actOnTerm(E, Right, Op);
    }
    return false;
  }
_error:
  while (!Tok.isOneOf(tok::r_paren, tok::plus,
                      tok::comma, tok::minus, tok::semi,
                      tok::less, tok::lessequal, tok::equal,
                      tok::greater, tok::greaterequal,
                      tok::kw_DO, tok::kw_else, tok::kw_end,
                      tok::kw_or, tok::kw_begin)) {
    advance();
    if (Tok.is(tok::eof))
      return true;
  }
  return false;
}

bool Parser::parseMulOperator(OperatorInfo &Op) {
  {
    if (Tok.is(tok::star)) {
      Op = fromTok(Tok);
      advance();
    } else if (Tok.is(tok::slash)) {
      Op = fromTok(Tok);
      advance();
    } else if (Tok.is(tok::kw_div)) {
      Op = fromTok(Tok);
      advance();
    } else if (Tok.is(tok::kw_mod)) {
      Op = fromTok(Tok);
      advance();
    } else if (Tok.is(tok::kw_and)) {
      Op = fromTok(Tok);
      advance();
    } else {
      /*ERROR*/
      goto _error;
    }
    return false;
  }
_error:
  while (!Tok.isOneOf(tok::l_paren,
                      tok::identifier,
                      tok::integer_literal)) {
    advance();
    if (Tok.is(tok::eof))
      return true;
  }
  return false;
}

bool Parser::parseFactor(Expr *&E) {
  {
    if (Tok.is(tok::integer_literal)) {
      E = Actions.actOnIntegerLiteral(Tok.getLocation(),
                                      Tok.getLiteralData());
      advance();
    } else if (Tok.is(tok::identifier)) {
      Decl *D;
      ExprList Exprs;
      if (Tok.is(tok::l_paren)) {
        advance();
        if (Tok.isOneOf(tok::l_paren, tok::plus, tok::minus,
                        tok::identifier, tok::integer_literal)) {
          if (parseExpList(Exprs))
            goto _error;
        }
        if (expect(tok::r_paren))
          goto _error;
        E = Actions.actOnFunctionCall(D, Exprs);
        advance();
      } else if (Tok.isOneOf(
                     tok::r_paren, tok::star,
                     tok::plus, tok::comma, tok::minus,
                     tok::slash, tok::semi, tok::less,
                     tok::lessequal, tok::equal,
                     tok::greater, tok::greaterequal,
                     tok::kw_and, tok::kw_div,
                     tok::kw_else, tok::kw_end, tok::kw_mod,
                     tok::kw_or, tok::kw_begin)) {
        E = Actions.actOnVariable(D);
      }
    } else if (Tok.is(tok::l_paren)) {
      advance();
      if (parseExpression(E))
        goto _error;
      if (consume(tok::r_paren))
        goto _error;
    } else {
      /*ERROR*/
      goto _error;
    }
    return false;
  }
_error:
  while (!Tok.isOneOf(
      tok::hash, tok::r_paren, tok::star, tok::plus,
      tok::comma, tok::minus, tok::slash, tok::semi,
      tok::less, tok::lessequal, tok::equal, tok::greater,
      tok::greaterequal, tok::kw_and, tok::kw_div,
      tok::kw_else, tok::kw_end, tok::kw_mod,
      tok::kw_or, tok::kw_begin)) {
    advance();
    if (Tok.is(tok::eof))
      return true;
  }
  return false;
}

bool Parser::parseEqualOps(Decl *&D) {
  {
    D = nullptr;
    if (expect(tok::identifier))
      goto _error;
    D = Actions.actOnQualIdentPart(D, Tok.getLocation(),
                                   Tok.getIdentifier());
    advance();
    while (Tok.is(tok::period) &&
           (isa<ModuleDeclaration>(D))) {
      advance();
      if (expect(tok::identifier))
        goto _error;
      D = Actions.actOnQualIdentPart(D, Tok.getLocation(),
                                     Tok.getIdentifier());
      advance();
    }
    return false;
  }
_error:
  while (!Tok.isOneOf(
      tok::hash, tok::l_paren, tok::r_paren, tok::star,
      tok::plus, tok::comma, tok::minus, tok::slash,
      tok::colonequal, tok::semi, tok::less, tok::lessequal,
      tok::equal, tok::greater, tok::greaterequal,
      tok::kw_and, tok::kw_div, tok::kw_else,
      tok::kw_end, tok::kw_mod, tok::kw_or, tok::kw_begin)) {
    advance();
    if (Tok.is(tok::eof))
      return true;
  }
  return false;
}

bool Parser::parseIdentList(IdentList &Ids) {
  {
    if (expect(tok::identifier))
      goto _error;
    Ids.push_back(std::pair<SMLoc, StringRef>(
        Tok.getLocation(), Tok.getIdentifier()));
    advance();
    while (Tok.is(tok::comma)) {
      advance();
      if (expect(tok::identifier))
        goto _error;
      Ids.push_back(std::pair<SMLoc, StringRef>(
          Tok.getLocation(), Tok.getIdentifier()));
      advance();
    }
    return false;
  }
_error:
  while (!Tok.isOneOf(tok::colon, tok::semi)) {
    advance();
    if (Tok.is(tok::eof))
      return true;
  }
  return false;
}