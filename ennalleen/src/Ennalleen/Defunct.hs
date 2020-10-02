{-# LANGUAGE GADTs #-}
module Ennalleen.Defunct where

import Ennalleen.BaseSyntax

-- | Defunctionalized expression
data Defunct where
  DVar :: Name -> Defunct
  DInt :: Int -> Defunct
  DBool :: Bool -> Defunct
  DTimes :: Defunct -> Defunct -> Defunct
  DPlus :: Defunct -> Defunct -> Defunct
  DMinus :: Defunct -> Defunct -> Defunct
  DEqual :: Defunct -> Defunct -> Defunct
  DLess :: Defunct -> Defunct -> Defunct
  DIf :: Defunct -> Defunct -> Defunct -> Defunct
  DApply :: [Defunct] -> Defunct
  DLet :: Name -> Defunct -> Defunct -> Defunct

-- | Function definition
data DefFun = DefFun
  { funName :: Name
  , args :: [Name]
  , body :: Defunct
  }

-- defunctionalize :: Expr -> Defunct
