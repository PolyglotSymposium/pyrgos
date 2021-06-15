{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
module Ennalleen.Kont
  ( Primitive(..), Func(..), Value(..), Expr(..)
  , eval
  ) where

data Primitive a where
  PStr :: String -> Primitive String
  PInt :: Int -> Primitive Int
  PBool :: Bool -> Primitive Bool

type Func f a b = f a -> f b

type Func2 f a b c = f a -> f b -> f c

data Value a where
  Prim :: Primitive a -> Value a
  PrimFunc :: Func Primitive i o -> Value (i -> o)
  PrimFunc2 :: Func2 Primitive i j o -> Value (i -> j -> o)
  Lambda :: Func Expr i o -> Value (i -> o)

data Expr a where
  Value :: Value a -> Expr a
  Apply :: Expr (i -> o) -> Expr i -> Expr o

evalApply :: Value (a -> b) -> Value a -> Value b
evalApply (Prim x) _ = case x of {}
evalApply (PrimFunc f) (Prim p) = Prim $ f p
evalApply (PrimFunc _) (PrimFunc _) = undefined -- error
evalApply (PrimFunc _) (Lambda _) = undefined -- error
evalApply (Lambda f) x = eval $ f $ Value x

eval :: Expr a -> Value a
eval (Apply f x) = evalApply (eval f) $ eval x
eval (Value v) = v

example1 :: Expr Int
example1 = Value (Prim (PInt 5))

example2 :: Expr (a -> b -> a)
example2 = Value $ Lambda $ \x -> Value $ Lambda $ \_ -> x

liftedConcat :: Primitive String -> Primitive String -> Primitive String
liftedConcat (PStr x) (PStr y) = PStr $ x ++ y

example3 :: Expr String
example3 = Apply (Apply (Value $ PrimFunc2 liftedConcat) (Value $ Prim $ PStr "foo")) (Value $ Prim $ PStr "bar")
