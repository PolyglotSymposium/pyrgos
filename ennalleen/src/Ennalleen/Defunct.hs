{-# LANGUAGE GADTs #-}
module Ennalleen.Defunct where

import Ennalleen.Syntax
import Control.Monad.Writer

-- | Function definition
data DefFun a = DefFun
  { funName :: Name
  , args :: [Name]
  , body :: a
  }

type Defunct a = Writer [DefFun a] a

-- | Defunctionalized expression
--data Defunct where
--  DVar :: Name -> Defunct
--  DInt :: Int -> Defunct
--  DBool :: Bool -> Defunct
--  DTimes :: Defunct -> Defunct -> Defunct
--  DPlus :: Defunct -> Defunct -> Defunct
--  DMinus :: Defunct -> Defunct -> Defunct
--  DEqual :: Defunct -> Defunct -> Defunct
--  DLess :: Defunct -> Defunct -> Defunct
--  DIf :: Defunct -> Defunct -> Defunct -> Defunct
--  DApply :: [Defunct] -> Defunct
--  DLet :: Name -> Defunct -> Defunct -> Defunct
