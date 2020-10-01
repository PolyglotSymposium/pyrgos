{-# LANGUAGE GADTs #-}
module Ennalleen.Syntax where

newtype Name = Name String -- TODO this is always the annoying part

data Ty where
  TInt :: Ty
  TBool :: Ty
  TFunc :: Ty -> Ty -> Ty

data Expr where
  EVar :: Name -> Expr
  EInt :: Int -> Expr
  EBool :: Bool -> Expr
  ETimes :: Expr -> Expr -> Expr
  EPlus :: Expr -> Expr -> Expr
  EMinus :: Expr -> Expr -> Expr
  EEqual :: Expr -> Expr -> Expr
  ELess :: Expr -> Expr -> Expr
  EIf :: Expr -> Expr -> Expr -> Expr
  ELambda :: Name -> Expr -> Expr
  EApply :: Expr -> Expr -> Expr
  ELet :: Name -> Expr -> Expr -> Expr
