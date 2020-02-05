{-# LANGUAGE DeriveFunctor #-}
-- | The meta/eval language's syntax
module Metacore.Eval.AST
  ( TopLevel(..), Terminal(..), Expr(..)
  ) where

import Data.Word (Word64)
import Control.Applicative (liftA2)
import Data.MExpr (MExpr(..), Deemify(..), Emify(..))
import Data.MExpr.Symbol (Symbol, symbol)

applSymbol :: Symbol
applSymbol = symbol 20 -- A

defineSymbol :: Symbol
defineSymbol = symbol 23 -- D

evalSymbol :: Symbol
evalSymbol = symbol 24 -- E

lambdaSymbol :: Symbol
lambdaSymbol = symbol 31 -- L

varSymbol :: Symbol
varSymbol = symbol 41 -- V

data Expr t              =
  Ap (Expr t) (Expr t)   |
  Lambda Word64 (Expr t) |
  T t                    |
  Var Word64             --
  deriving Functor

instance Emify t => Emify (Expr t) where
  emify = \case
    Ap f x -> MExpr applSymbol [emify f, emify x]
    Lambda arg body -> MExpr lambdaSymbol [SymLit arg, emify body]
    T t -> emify t
    Var x -> MExpr varSymbol [SymLit x]

data Terminal                   =
  Char Char                     |
  Nat Integer                   |
  String String                 |
  Symbol Word64                 --

instance Emify Terminal where
  emify = \case
    Char c -> ChrLit c
    Nat n -> NatLit n
    String s -> StrLit s
    Symbol s -> SymLit s

data TopLevel
  = Def Word64 (Expr Terminal)
  | Eval (Expr Terminal)

-- TODO property: deemify (emify t) = Just t
instance Deemify Terminal where
  deemify = \case
    StrLit s -> Just $ String s
    ChrLit c -> Just $ Char c
    NatLit i -> Just $ Nat i
    SymLit s -> Just $ Symbol s
    _ -> Nothing

instance Deemify t => Deemify (Expr t) where
  deemify = \case
    MExpr m [e1, e2] | m == applSymbol ->
      liftA2 Ap (deemify e1) (deemify e2)
    MExpr m [SymLit s] | m == varSymbol ->
      Just $ Var s
    MExpr m [SymLit e1, e2] | m == lambdaSymbol ->
      Lambda e1 <$> deemify e2
    x -> T <$> deemify x

instance Deemify TopLevel where
  deemify = \case
    MExpr m [SymLit s, e] | m == defineSymbol ->
      Def s <$> deemify e
    MExpr m [e] | m == evalSymbol ->
      Eval <$> deemify e
    _ -> Nothing
