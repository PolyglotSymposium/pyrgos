module Expr
  ( Literal(..), literalType
  , Expr(..)
  , Decl(..)
  , TopLevel(..)
  ) where

type Name = String

data Literal    =
  IntLit Int    |
  StringLit String --
  deriving Show

literalType :: Literal -> Name
literalType (IntLit _) ="Int"
literalType (StringLit _) ="String"

data Expr             =
  Var Name            |
  Lit Literal         |
  Apply Expr Expr     |
  Lambda String Expr  |
  Let Name Expr Expr  --
  deriving Show

data Decl          =
  Define Name Expr --

data TopLevel     =
  TLExpr Expr     |
  TLDecl Decl     --
