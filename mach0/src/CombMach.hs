{-# LANGUAGE BlockArguments #-}
-- | An instruction set implementing a stack machine for a combinator
-- | calculus
module CombMach
  ( Expr -- abstract type
  , atom, appl -- constructors for Expr
  , sComb, kComb -- constructors for Expr
  , primFun1, primFun2, primFun3 -- constructors for Expr
  , Instr, step1
  ) where

import Mach0

type CombMachSt a = MachSt (Expr a) Instr
type CombMach a = Mach0 (Expr a) Instr

type PrimFun1 a = Expr a                     -> CombMachSt a
type PrimFun2 a = Expr a           -> Expr a -> CombMachSt a
type PrimFun3 a = Expr a -> Expr a -> Expr a -> CombMachSt a

data Closure a                 =
  ZeroOf1 (PrimFun1 a)         |
  ZeroOf2 (PrimFun2 a)         |
  OneOf2 (PrimFun2 a) (Expr a) |
  ZeroOf3 (PrimFun3 a)         |
  OneOf3 (PrimFun3 a) (Expr a) |
  TwoOf3 (PrimFun3 a) (Expr a) (Expr a)

data Expr a              =
  Atom a                 |
  Clos (Closure a)       |
  Appl (Expr a) (Expr a) --

atom :: a -> Expr a
atom = Atom

appl :: Expr a -> Expr a -> Expr a
appl = Appl

primFun1 :: PrimFun1 a -> Expr a
primFun1 = Clos . ZeroOf1

primFun2 :: PrimFun2 a -> Expr a
primFun2 = Clos . ZeroOf2

primFun3 :: PrimFun3 a -> Expr a
primFun3 = Clos . ZeroOf3

apply :: Monad m => Closure a -> Expr a -> CombMach a m ()
apply (ZeroOf1 f) x = fill $ f x
apply (ZeroOf2 f) x = push $ Clos $ OneOf2 f x
apply (OneOf2 f x1) x2 = fill $ f x1 x2
apply (ZeroOf3 f) x = push $ Clos $ OneOf3 f x
apply (OneOf3 f x1) x2 = push $ Clos $ TwoOf3 f x1 x2
apply (TwoOf3 f x1 x2) x3 = fill $ f x1 x2 x3

data Instr =
  Call     |
  Eval     |
  Load     |
  Save     --

sComb :: Expr a
sComb = primFun3 \f g x ->
  instrs [Call, Save, Call, Load, Call]
  <> values [f, x, g, x]

kComb :: Expr a
kComb = primFun2 \x _ -> values [x]

nonTerminal :: Expr a -> Maybe (CombMachSt a)
nonTerminal (Appl f x) =
  Just (instrs [Eval, Save, Eval, Load, Call]
        <> values [f, x])
nonTerminal _ = Nothing

closure :: Monad m => Expr a -> CombMach a m (Closure a)
closure (Clos c) = pure c
closure _ = freeze

eval :: Monad m => CombMach a m ()
eval = do
  e <- pop
  maybe (pure ()) fill $ nonTerminal e

call :: Monad m => CombMach a m ()
call = do
  f <- pop
  c <- closure f
  x <- pop
  apply c x

exec1 :: Monad m => Instr -> CombMach a m ()
exec1 Call = call
exec1 Eval = eval
exec1 Load = load
exec1 Save = save

step1 :: Monad m => CombMach a m ()
step1 = erase >>= exec1
