module Semantics.Intermediate where


import Base
--import Grammar.Abstract (Var, SgPl, Expr, Operator, Chain, Pattern)
import Grammar.Abstract hiding (Stmt)

import qualified Grammar.Abstract as Gram




data Stmt
   = StmtFormula Formula
   | StmtNeg Stmt
   | StmtAttr Term Attr
   | StmtVerb Term Verb
   | StmtNotion Term Notion
   | StmtExists Notion
   | StmtConj Conj Stmt Stmt
   | All  (NonEmpty Var) (Maybe Notion) Stmt
   | Most (NonEmpty Var) (Maybe Notion) Stmt
   | Some (NonEmpty Var) (Maybe Notion) Stmt
   | None (NonEmpty Var) (Maybe Notion) Stmt
   | Uniq (NonEmpty Var) (Maybe Notion) Stmt
   deriving (Show, Eq)