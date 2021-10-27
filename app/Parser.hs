module Parser where

import CommonParserUtil
import Token

data Expr = NoExpr
  deriving Show

noAction rhs = NoExpr

parserSpec :: ParserSpec Token Expr
parserSpec = ParserSpec
  {
    startSymbol = "Prog'",
    
    parserSpecList =
    [
      ("Prog' -> Prog", noAction),
      ("Prog -> MoreThanOneStmt", noAction),
      ("Stmt -> ExprStatement", noAction),
      ("Stmt -> While Expr CRStmtCRs EndWhile", noAction),
      ("Stmt -> ID :", noAction),
      ("Stmt -> Goto ID", noAction),
      ("Stmt -> For ID = Expr To Expr OptStep CRStmtCRs EndFor", noAction),
      ("Stmt -> Sub ID CRStmtCRs EndSub", noAction),
      ("Stmt -> If Expr Then CRStmtCRs MoreThanZeroElseIf", noAction),
      ("Stmt -> ", noAction),
      ("MoreThanZeroElseIf -> OptionalElse", noAction),
      ("MoreThanZeroElseIf -> ElseIf Expr Then CRStmtCRs MoreThanZeroElseIf", noAction),
      ("OptionalElse -> EndIf", noAction),
      ("OptionalElse -> Else CRStmtCRs EndIf", noAction),
      ("ExprStatement -> ID = Expr", noAction),
      ("ExprStatement -> ID . ID = Expr", noAction),
      ("ExprStatement -> ID . ID ( Exprs )", noAction),
      ("ExprStatement -> ID ( )", noAction),
      ("ExprStatement -> ID Idxs = Expr", noAction),
      ("CRStmtCRs -> CR TheRest", noAction),
      ("TheRest ->", noAction),
      ("TheRest -> Stmt CR TheRest", noAction),
      ("MoreThanOneStmt -> Stmt", noAction),
      ("MoreThanOneStmt -> Stmt CR MoreThanOneStmt", noAction),
      ("OptStep ->", noAction),
      ("OptStep -> Step Expr", noAction),
      ("Expr -> CondExpr", noAction),
      ("Exprs ->", noAction),
      ("Exprs -> MoreThanOneExpr", noAction),
      ("MoreThanOneExpr -> Expr", noAction),
      ("MoreThanOneExpr -> Expr , MoreThanOneExpr", noAction),
      ("CondExpr -> OrExpr", noAction),
      ("OrExpr -> OrExpr Or AndExpr", noAction),
      ("OrExpr -> AndExpr", noAction),
      ("AndExpr -> AndExpr And EqNeqExpr", noAction),
      ("AndExpr -> EqNeqExpr", noAction),
      ("EqNeqExpr -> EqNeqExpr = CompExpr", noAction),
      ("EqNeqExpr -> EqNeqExpr <> CompExpr", noAction),
      ("EqNeqExpr -> CompExpr", noAction),
      ("CompExpr -> CompExpr < AdditiveExpr", noAction),
      ("CompExpr -> CompExpr <= AdditiveExpr", noAction),
      ("CompExpr -> CompExpr > AdditiveExpr", noAction),
      ("CompExpr -> CompExpr >= AdditiveExpr", noAction),
      ("CompExpr -> AdditiveExpr", noAction),
      ("AdditiveExpr -> AdditiveExpr + MultiplicativeExpr", noAction),
      ("AdditiveExpr -> AdditiveExpr - MultiplicativeExpr", noAction),
      ("AdditiveExpr -> MultiplicativeExpr", noAction),
      ("MultiplicativeExpr -> MultiplicativeExpr * UnaryExpr", noAction),
      ("MultiplicativeExpr -> MultiplicativeExpr / UnaryExpr", noAction),
      ("MultiplicativeExpr -> UnaryExpr", noAction),
      ("UnaryExpr -> - Primary", noAction),
      ("UnaryExpr -> Primary", noAction),
      ("Primary -> NUM", noAction),
      ("Primary -> STR", noAction),
      ("Primary -> ( Expr )", noAction),
      ("Primary -> ID", noAction),
      ("Primary -> ID . ID", noAction),
      ("Primary -> ID . ID ( Exprs )", noAction),
      ("Primary -> ID Idxs", noAction),
      ("Idxs -> [ Expr ]", noAction),
      ("Idxs -> [ Expr ] Idxs", noAction)
    ],
    
    baseDir = "./",
    actionTblFile = "action_table.txt",  
    gotoTblFile = "goto_table.txt",
    grammarFile = "prod_rules.txt",
    parserSpecFile = "mygrammar.grm",
    genparserexe = "yapb-exe"
  }

  
