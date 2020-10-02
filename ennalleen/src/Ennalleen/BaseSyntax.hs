{-# LANGUAGE GADTs #-}
module Ennalleen.BaseSyntax where

newtype Name =
  Name String -- TODO this is always the annoying part
  deriving Show

data Ty where
  TInt :: Ty
  TBool :: Ty
  TFunc :: Ty -> Ty -> Ty
  deriving Show

data BinOp = Times | Plus | Minus | Equal | Less deriving Show
