-- | Untyped lambda calculus
module Lambda (LExpr(..)) where

data LExpr a                =
  Atom a                    |
  Var Int                   |
  Lambda (LExpr a)          |
  Apply (LExpr a) (LExpr a)
