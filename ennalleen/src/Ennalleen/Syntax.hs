{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
module Ennalleen.Syntax where

import Control.Monad.Free (wrap)
import Control.Monad.Free.Church (F)

newtype Name =
  Name String -- TODO this is always the annoying part
  deriving Show

data Ty where
  TInt :: Ty
  TBool :: Ty
  TFunc :: Ty -> Ty -> Ty
  deriving Show

data BinOp = Times | Plus | Minus | Equal | Less deriving Show

class InjectVar a where
  injectVar :: Name -> a

-- | A first-order syntax for the lambda calculus
class InjectVar a => InjectLCFOAS a where
  injectLambda :: Name -> a -> a
  injectApply :: a -> a -> a

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
instance InjectVar String where
  injectVar (Name n) = n

-- For debugging
instance InjectLCFOAS String where
  injectLambda (Name n) x = "(" ++ n ++ " => " ++ x ++ ")"
  injectApply f x = "(" ++ f ++ " " ++ x ++ ")"

-- For debugging
instance InjectInt String where
  injectInt = show

-- For debugging
instance InjectLet String where
  injectLet (Name n) x b = "(let " ++ n ++ " = " ++ x ++ " in " ++ b ++ ")"

-- For debugging
instance InjectOp2 String where
  injectOp2 op x y = "(" ++ x ++ " " ++ show op ++ " " ++ y ++ ")"

-- For debugging
instance InjectIf String where
  injectIf cond t e = "(if " ++ cond ++ " then " ++ t ++ " else " ++ e ++ ")"

-- For debugging
instance ParsedExpr String

-- | A first-order syntax for the lambda calculus
data LCFOAS a where
  Lambda :: Name -> a -> LCFOAS a
  Apply :: a -> a -> LCFOAS a
  deriving Functor

instance InjectVar a => InjectVar (F f a) where
  injectVar = return . injectVar

instance InjectInt a => InjectInt (F f a) where
  injectInt = return . injectInt

--instance InjectLet a => InjectLet (F f a) where
--  injectLet n x b = return $ injectLet n x b

--instance InjectOp2 (F f a) where
--  injectOp2 op x y = _

--instance InjectIf (F f a) where
--  injectIf cond t e = _

instance InjectVar a => InjectLCFOAS (F LCFOAS a) where
  injectLambda x b = wrap $ Lambda x b
  injectApply f x = wrap $ Apply f x
