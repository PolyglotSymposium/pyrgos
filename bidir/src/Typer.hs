{-# LANGUAGE FlexibleContexts #-}
module Typer where

import Control.Monad.Error.Class
import Control.Monad.State.Class
import Control.Monad (guard)

import AST
import Context

subtypeOf :: (MonadState Context m, MonadError String m)
          => Polytype -> Polytype -> m ()
-- <:Unit
subtypeOf (PolyTerminalType UnitType) (PolyTerminalType UnitType) =
  return ()
-- <:Var
subtypeOf x'@(PolyTerminalType (UniversalTypeVar x)) (PolyTerminalType (UniversalTypeVar y)) = do
  maybe (throwError "Unable to prove subtyping relation for unrelated universal type variables") return $ guard (x == y)
  wellFormedPolytype x'
-- <:Exvar
subtypeOf x'@(PolyTerminalType (ExistentialTypeVar x)) (PolyTerminalType (ExistentialTypeVar y)) = do
  maybe (throwError "Unable to prove subtyping relation for unrelated existential type variables") return $ guard (x == y)
  wellFormedPolytype x'
-- <:→
subtypeOf (PolyFunctionType aIn aOut) (PolyFunctionType bIn bOut) = do
  subtypeOf bIn aIn
  substitutions <- get
  let aOut' = substituteSolved substitutions aOut
  let bOut' = substituteSolved substitutions bOut
  subtypeOf aOut' bOut'
-- Should the left for-all rule get precedence over the right?
-- That's what we've done here.
-- <:∀L
subtypeOf (Forall alpha a) b = do
  let newExistential = "TODO generate fresh type variables"
  extendWithUnsolved newExistential
  let a' = substituteForUniversal alpha (ExistentialTypeVar newExistential) a
  subtypeOf a' b
  truncateContextE newExistential
-- <:∀R
subtypeOf a (Forall alpha b) = do
  extendWithUniversal alpha
  subtypeOf a b
  truncateContextA alpha
subtypeOf _sub _sup = undefined -- TODO

instantiateL :: (MonadState Context m, MonadError String m)
             => Name -> Polytype -> m ()
instantiateL _name (PolyTerminalType _tt) = undefined
instantiateL _name (Forall _a _b) = undefined
-- InstLArr
instantiateL name (PolyFunctionType _a1 _a2) = do
  refineExistialAsFunction name
  undefined

instantiateR :: (MonadState Context m, MonadError String m)
             => Polytype -> Name -> m ()
instantiateR (PolyTerminalType _tt) _name = undefined
instantiateR (Forall _a _b) _name = undefined
-- InstRArr
instantiateR (PolyFunctionType _a _b) name = do
  refineExistialAsFunction name
  undefined
