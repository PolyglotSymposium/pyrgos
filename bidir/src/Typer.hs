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

isExistentialInScope :: Monotype ->

instantiateL :: (MonadState Context m, MonadError String m)
             => Name -> Polytype -> m ()
-- InstLAllR
instantiateL name (Forall beta b) = do
  wellFormedPolytype (PolyTerminalType (ExistentialTypeVar name)) -- TODO really? Every time?
  extendWithUniversal beta -- TODO do we need a with pattern?
  instantiateL name b
  truncateContextA beta
-- Solve: Γ,α^,Γ'
-- Reach: Γ[α^][β^]
-- Reach: Γ,α^,Γ'[β^]
-- Solve: τ
-- Reach: τ = β^
-- Is tau an existential variable? If so, is in Γ'? If both are reach, apply
-- the reach rule. If either one of those is not true, apply solve, and τ must
-- be well-formed w.r.t. Γ.
instantiateL name poly =
  case polyIsMono poly of
    Nothing => -- TODO felt overwhelmed and called it a day
    Just mono =>
  get <- context
  let (gamma', gamma) = splitContextE name context
  -- Is tau an existential variable that is in scope in gamma'?
  let whichRule = do
    beta <- monotypeIsExistential tau
    either (\_ -> Nothing) Just $ runStateT gamma' $ wellFormedPolytype $ PolyTerminalType $ ExistentialTypeVar beta
  case whichRule do
    -- InstLSolve
    Nothing => undefined
    -- InstLReach
    Just () => undefined
instantiateL name (PolyFunctionType a1 a2) = instantiateLArrow name a1 a2

-- InstLArr
instantiateLArrow :: (MonadState Context m, MonadError String m)
                  => Name -> Polytype -> Polytype -> m ()
instantiateLArrow name a1 a2 = do
  (alpha1, alpha2) <- refineExistialAsFunction name
  instantiateR a1 alpha1
  substitutions <- get
  let a2' = substituteSolved substitutions a2
  instantiateL alpha2 a2'

instantiateR :: (MonadState Context m, MonadError String m)
             => Polytype -> Name -> m ()
-- InstRAllL
instantiateR (Forall beta b) name = do
  wellFormedPolytype (PolyTerminalType (ExistentialTypeVar name)) -- TODO really? Every time?
  let newExistential = "TODO generate fresh type variables"
  extendWithUnsolved newExistential
  let b' = substituteForUniversal beta (ExistentialTypeVar newExistential) b
  instantiateL name b'
  truncateContextE newExistential
instantiateR (PolyTerminalType _tt) _name = undefined
instantiateR (PolyFunctionType a1 a2) name = instantiateRArrow a1 a2 name

-- InstRArr
instantiateRArrow :: (MonadState Context m, MonadError String m)
                  => Polytype -> Polytype -> Name -> m ()
instantiateRArrow a1 a2 name = do
  (alpha1, alpha2) <- refineExistialAsFunction name
  instantiateL alpha1 a1
  substitutions <- get
  let a2' = substituteSolved substitutions a2
  instantiateR a2' alpha2
