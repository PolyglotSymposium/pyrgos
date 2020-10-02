{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
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

-- | A first-order syntax for the lambda calculus
class InjectLCFOAS a where
  injectLambda :: Name -> a -> a
  injectAppl :: a -> a -> a
  injectVar :: Name -> a

class InjectBool a where
  injectBool :: Bool -> a

class InjectInt a where
  injectInt :: Int -> a

class (InjectBool a, InjectBool a) => InjectAtom a

class InjectLet a where
  injectLet :: Name -> a -> a -> a

class InjectOp2 a where
  injectOp2 :: BinOp -> a -> a -> a

class InjectIf a where
  injectIf :: a -> a -> a -> a

class ( InjectLet a
      , InjectLCFOAS a
      , InjectInt a
      , InjectIf a
      , InjectOp2 a
      ) => ParsedExpr a

-- For debugging
instance InjectLCFOAS String where
  injectLambda n x = "(" ++ show n ++ " => " ++ x ++ ")"
  injectAppl f x = "(" ++ f ++ " " ++ x ++ ")"
  injectVar n = show n

-- For debugging
instance InjectInt String where
  injectInt = show

-- For debugging
instance InjectLet String where
  injectLet n x b = "(let " ++ show n ++ " = " ++ x ++ " in " ++ b ++ ")"

-- For debugging
instance InjectOp2 String where
  injectOp2 op x y = "(" ++ x ++ " " ++ show op ++ " " ++ y ++ ")"

-- For debugging
instance InjectIf String where
  injectIf cond t e = "(if " ++ cond ++ " then " ++ t ++ " else " ++ e ++ ")"

-- For debugging
instance ParsedExpr String
