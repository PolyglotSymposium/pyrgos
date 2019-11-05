-- | The meta/core language's syntax
module Metacore.AST where

import Control.Applicative (liftA2)
import Data.MExpr (MExpr(..))
import Data.MExpr.Symbol (Symbol, avowSymbol)
import Text.Read (readMaybe)

data Expr
  = Var String
  | Lam String Expr
  | Ap Expr Expr
  | String String
  | Symbol Word
  | Nat Integer
  deriving Show

v :: Symbol
v = avowSymbol "v"

l :: Symbol
l = avowSymbol "l"

a :: Symbol
a = avowSymbol "a"

s :: Symbol
s = avowSymbol "s"

i :: Symbol
i = avowSymbol "i"

n :: Symbol
n = avowSymbol "n"

ofMExpr :: MExpr -> Maybe Expr
ofMExpr (Literal _) = Nothing
ofMExpr (MExpr f xs) =
  if f == v
  then case xs of
         [Literal x] -> Just $ Var x
         _ -> Nothing
  else if f == l
  then case xs of
         [Literal param, body] -> Lam param <$> ofMExpr body
         _ -> Nothing
  else if f == a
  then case xs of
         [f', x] -> liftA2 Ap (ofMExpr f') (ofMExpr x)
         _ -> Nothing
  else if f == s
  then case xs of
         [Literal x] -> Just $ String x
         _ -> Nothing
  else if f == i
  then case xs of
         [Literal x] -> Symbol <$> readMaybe x
         _ -> Nothing
  else if f == n
  then case xs of
         [Literal x] -> Symbol <$> readMaybe x
         _ -> Nothing
  else Nothing
