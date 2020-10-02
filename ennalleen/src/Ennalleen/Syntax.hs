{-# LANGUAGE GADTs #-}
module Ennalleen.Syntax where

import Ennalleen.BaseSyntax
import Ennalleen.Parser

data Atom where
  ABool :: Bool -> Atom
  AInt :: Int -> Atom
  deriving Show

data Expr where
  EVar :: Name -> Expr
  EAtom :: Atom -> Expr
  EBinOp :: BinOp -> Expr -> Expr -> Expr
  EIf :: Expr -> Expr -> Expr -> Expr
  ELambda :: Name -> Expr -> Expr
  EApply :: Expr -> Expr -> Expr
  ELet :: Name -> Expr -> Expr -> Expr
  deriving Show

instance ParsedExpr Expr where
  injectLet = ELet
  injectVar = EVar
  injectInt = EAtom . AInt
  injectLam = ELambda
  injectIfT = EIf
  injectAp = EApply
  injectOp = EBinOp
