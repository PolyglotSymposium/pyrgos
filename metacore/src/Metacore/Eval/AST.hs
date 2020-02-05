{-# LANGUAGE DeriveFunctor #-}
-- | The meta/eval language's syntax
module Metacore.Eval.AST
  ( TopLevel(..), Terminal(..), Expr(..)
  ) where

import Control.Applicative (liftA2)
import Data.MExpr (Symbol(..), MExpr(..), Deemify(..), Emify(..))

applSymbol :: Symbol
applSymbol = Symbol 20 -- A

defineSymbol :: Symbol
defineSymbol = Symbol 23 -- D

evalSymbol :: Symbol
evalSymbol = Symbol 24 -- E

lambdaSymbol :: Symbol
lambdaSymbol = Symbol 31 -- L

varSymbol :: Symbol
varSymbol = Symbol 41 -- V

data Expr t              =
  Ap (Expr t) (Expr t)   |
  Lambda Symbol (Expr t) |
  T t                    |
  Var Symbol             --
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
  TSymbol Symbol                --

instance Emify Terminal where
  emify = \case
    Char c -> ChrLit c
    Nat n -> NatLit n
    String s -> StrLit s
    TSymbol s -> SymLit s

data TopLevel
  = Def Symbol (Expr Terminal)
  | Eval (Expr Terminal)

-- TODO property: deemify (emify t) = Just t
instance Deemify Terminal where
  deemify = \case
    StrLit s -> Just $ String s
    ChrLit c -> Just $ Char c
    NatLit i -> Just $ Nat i
    SymLit s -> Just $ TSymbol s
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
