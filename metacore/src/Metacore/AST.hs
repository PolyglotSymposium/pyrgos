-- | The meta/core language's syntax
module Metacore.AST (Expr, ofMExpr) where

import Data.Word (Word64)
import Control.Applicative (liftA2)
import Data.MExpr (MExpr(..))
import Data.MExpr.Symbol (Symbol, avowSymbol)

data Expr
  = Var String
  | Lam String Expr
  | Ap Expr Expr
  | String String
  | Symbol Word64
  | Nat Integer
  | Char Char
  deriving Show

v :: Symbol
v = avowSymbol "v"

l :: Symbol
l = avowSymbol "l"

a :: Symbol
a = avowSymbol "a"

ofMExpr :: MExpr -> Maybe Expr
ofMExpr (StrLit s) = Just $ String s
ofMExpr (ChrLit c) = Just $ Char c
ofMExpr (NatLit i) = Just $ Nat i
ofMExpr (SymLit s) = Just $ Symbol s
ofMExpr (MExpr f xs) =
  if f == v
  then case xs of
         [StrLit x] -> Just $ Var x
         _ -> Nothing
  else if f == l
  then case xs of
         [StrLit param, body] -> Lam param <$> ofMExpr body
         _ -> Nothing
  else if f == a
  then case xs of
         [f', x] -> liftA2 Ap (ofMExpr f') (ofMExpr x)
         _ -> Nothing
  else Nothing
