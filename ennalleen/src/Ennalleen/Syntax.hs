{-# LANGUAGE GADTs #-}
module Ennalleen.Syntax where

newtype Name =
  Name String -- TODO this is always the annoying part
  deriving Show

data Ty where
  TInt :: Ty
  TBool :: Ty
  TFunc :: Ty -> Ty -> Ty
  deriving Show

data BinOp = Times | Plus | Minus | Equal | Less deriving Show

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
