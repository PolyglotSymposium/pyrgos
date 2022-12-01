{-# LANGUAGE FlexibleContexts #-}
module Typer where

import Control.Monad.Error.Class
import Control.Monad.State.Class
import Control.Monad (guard)

import AST
import Context

subtypeOf :: (MonadState Context m, MonadError String m)
          => Polytype -> Polytype -> m ()
subtypeOf (PolyTerminalType UnitType) (PolyTerminalType UnitType) =
  return ()
subtypeOf x'@(PolyTerminalType (UniversalTypeVar x)) (PolyTerminalType (UniversalTypeVar y)) = do
  maybe (throwError "Unable to prove subtyping relation for unrelated universal type variables") return $ guard (x == y)
  wellFormedPolytype x'
subtypeOf x'@(PolyTerminalType (ExistentialTypeVar x)) (PolyTerminalType (ExistentialTypeVar y)) = do
  maybe (throwError "Unable to prove subtyping relation for unrelated existential type variables") return $ guard (x == y)
  wellFormedPolytype x'
subtypeOf (PolyFunctionType aIn aOut) (PolyFunctionType bIn bOut) = do
  subtypeOf bIn aIn
  substitutions <- get
  let aOut' = substituteSolved substitutions aOut
  let bOut' = substituteSolved substitutions bOut
  subtypeOf aOut' bOut'
-- Should the left for-all rule get precedence over the right?
-- That's what we've done here.
subtypeOf (Forall alpha a) b = do
  let newExistential = "TODO generate fresh type variables"
  extendWithUnsolved newExistential
  let a' = substituteForUniversal alpha (ExistentialTypeVar newExistential) a
  subtypeOf a' b
  truncateContext newExistential
subtypeOf _sub _sup = undefined -- TODO
