-- | The meta/eval language's syntax
module Metacore.Eval.AST
  ( TopLevel(..), Fun(..), Terminal(..), Expr(..)
  , mExprToTopLevel
  ) where

import Data.Word (Word64)
import Control.Applicative (liftA2)
import Data.MExpr (MExpr(..))
import Data.MExpr.Symbol (Symbol, avowSymbol)

data Terminal
  = String String
  | Symbol Word64
  | Nat Integer
  | Char Char
  deriving Show

data Expr
  = Ap Expr Expr
  | T Terminal
  | Var Word64
  deriving Show

data Fun = Fun Word64 Expr

data TopLevel
  = DefVar Word64 Expr
  | DefFun Word64 Fun
  | Eval Expr

applSymbol :: Symbol
applSymbol = avowSymbol "a"

defineSymbol :: Symbol
defineSymbol = avowSymbol "d"

evalSymbol :: Symbol
evalSymbol = avowSymbol "e"

varSymbol :: Symbol
varSymbol = avowSymbol "v"

mExprToExpr :: MExpr -> Maybe Expr
mExprToExpr = \case
  StrLit s -> Just $ T $ String s
  ChrLit c -> Just $ T $ Char c
  NatLit i -> Just $ T $ Nat i
  SymLit s -> Just $ T $ Symbol s
  MExpr m [e1, e2] | m == applSymbol ->
    liftA2 Ap (mExprToExpr e1) (mExprToExpr e2)
  MExpr m [SymLit s] | m == varSymbol -> Just $ Var s
  _ -> Nothing

mExprToTopLevel :: MExpr -> Maybe TopLevel
mExprToTopLevel = \case
  MExpr m [SymLit s, e] | m == defineSymbol ->
    DefVar s <$> mExprToExpr e
  MExpr m [SymLit s, SymLit a, e] | m == defineSymbol ->
    DefFun s . Fun a <$> mExprToExpr e
  MExpr m [e] | m == evalSymbol ->
    Eval <$> mExprToExpr e
  _ -> Nothing
