module Expr (Expr(..)) where

type Name = String

data Expr             =
  Var Name            |
  Apply Expr Expr     |
  Lambda String Expr  |
  Let Name Expr Expr  --
  deriving Show
