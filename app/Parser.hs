module Parser where

import CommonParserUtil
import Token
import Expr

-- | Utility
rule prodRule action              = (prodRule, action, Nothing  )
ruleWithPrec prodRule action prec = (prodRule, action, Just prec)
ruleWithNoAction prodRule         = (prodRule, noAction, Nothing)

noAction rhs = NoExpr

--
parserSpec :: ParserSpec Token Expr
parserSpec = ParserSpec
  {
    startSymbol = "Prog'",
    
    tokenPrecAssoc = [],
    
    parserSpecList =
    [
      ruleWithNoAction "Prog' -> Prog",
      ruleWithNoAction "Prog -> MoreThanOneStmt",
      ruleWithNoAction "Stmt -> ExprStatement",
      ruleWithNoAction "Stmt -> While Expr CRStmtCRs EndWhile",
      ruleWithNoAction "Stmt -> ID :",
      ruleWithNoAction "Stmt -> Goto ID",
      ruleWithNoAction "Stmt -> For ID = Expr To Expr OptStep CRStmtCRs EndFor",
      ruleWithNoAction "Stmt -> Sub ID CRStmtCRs EndSub",
      ruleWithNoAction "Stmt -> If Expr Then CRStmtCRs MoreThanZeroElseIf",
      ruleWithNoAction "Stmt -> ",
      ruleWithNoAction "MoreThanZeroElseIf -> OptionalElse",
      ruleWithNoAction "MoreThanZeroElseIf -> ElseIf Expr Then CRStmtCRs MoreThanZeroElseIf",
      ruleWithNoAction "OptionalElse -> EndIf",
      ruleWithNoAction "OptionalElse -> Else CRStmtCRs EndIf",
      ruleWithNoAction "ExprStatement -> ID = Expr",
      ruleWithNoAction "ExprStatement -> ID . ID = Expr",
      ruleWithNoAction "ExprStatement -> ID . ID ( Exprs )",
      ruleWithNoAction "ExprStatement -> ID ( )",
      ruleWithNoAction "ExprStatement -> ID Idxs = Expr",
      ruleWithNoAction "CRStmtCRs -> CR TheRest",
      ruleWithNoAction "TheRest ->",
      ruleWithNoAction "TheRest -> Stmt CR TheRest",
      ruleWithNoAction "MoreThanOneStmt -> Stmt",
      ruleWithNoAction "MoreThanOneStmt -> Stmt CR MoreThanOneStmt",
      ruleWithNoAction "OptStep ->",
      ruleWithNoAction "OptStep -> Step Expr",
      ruleWithNoAction "Expr -> CondExpr",
      ruleWithNoAction "Exprs ->",
      ruleWithNoAction "Exprs -> MoreThanOneExpr",
      ruleWithNoAction "MoreThanOneExpr -> Expr",
      ruleWithNoAction "MoreThanOneExpr -> Expr , MoreThanOneExpr",
      ruleWithNoAction "CondExpr -> OrExpr",
      ruleWithNoAction "OrExpr -> OrExpr Or AndExpr",
      ruleWithNoAction "OrExpr -> AndExpr",
      ruleWithNoAction "AndExpr -> AndExpr And EqNeqExpr",
      ruleWithNoAction "AndExpr -> EqNeqExpr",
      ruleWithNoAction "EqNeqExpr -> EqNeqExpr = CompExpr",
      ruleWithNoAction "EqNeqExpr -> EqNeqExpr <> CompExpr",
      ruleWithNoAction "EqNeqExpr -> CompExpr",
      ruleWithNoAction "CompExpr -> CompExpr < AdditiveExpr",
      ruleWithNoAction "CompExpr -> CompExpr <= AdditiveExpr",
      ruleWithNoAction "CompExpr -> CompExpr > AdditiveExpr",
      ruleWithNoAction "CompExpr -> CompExpr >= AdditiveExpr",
      ruleWithNoAction "CompExpr -> AdditiveExpr",
      ruleWithNoAction "AdditiveExpr -> AdditiveExpr + MultiplicativeExpr",
      ruleWithNoAction "AdditiveExpr -> AdditiveExpr - MultiplicativeExpr",
      ruleWithNoAction "AdditiveExpr -> MultiplicativeExpr",
      ruleWithNoAction "MultiplicativeExpr -> MultiplicativeExpr * UnaryExpr",
      ruleWithNoAction "MultiplicativeExpr -> MultiplicativeExpr / UnaryExpr",
      ruleWithNoAction "MultiplicativeExpr -> UnaryExpr",
      ruleWithNoAction "UnaryExpr -> - Primary",
      ruleWithNoAction "UnaryExpr -> Primary",
      ruleWithNoAction "Primary -> NUM",
      ruleWithNoAction "Primary -> STR",
      ruleWithNoAction "Primary -> ( Expr )",
      ruleWithNoAction "Primary -> ID",
      ruleWithNoAction "Primary -> ID . ID",
      ruleWithNoAction "Primary -> ID . ID ( Exprs )",
      ruleWithNoAction "Primary -> ID Idxs",
      ruleWithNoAction "Idxs -> [ Expr ]",
      ruleWithNoAction "Idxs -> [ Expr ] Idxs"
    ],
    
    baseDir = "./",
    actionTblFile = "action_table.txt",  
    gotoTblFile = "goto_table.txt",
    grammarFile = "prod_rules.txt",
    parserSpecFile = "mygrammar.grm",
    genparserexe = "yapb-exe"
  }

  
