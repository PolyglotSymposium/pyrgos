{-# LANGUAGE FlexibleContexts #-}
module Typer
  ( subtypeOf
  , instantiateL, instantiateR
  , instantiateLArrow
  ) where

import Control.Monad.Error.Class
import Control.Monad.State.Class
import Control.Monad.Trans.State (evalStateT)
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

-- | The various instantiate rules are disjoint, but for the most part, not
-- | trivially so.
-- |
-- | 1. `InstLAllR` is syntactically disjoint from the three other
-- |     left-instantiation rules---and likewise `InstRAllL` from the other
-- |     right-instantiation rules---on the type AST alone, since a monotype
-- |     cannot include a ∀ binding.
-- | 2. InstLArr and InstLReach (and likewise InstRArr and InstRReach) are
-- |    syntactically disjoint from one another, because a polytype function and an
-- |    existential variable are syntactically disjoint.
-- | 3. InstLSolve is not syntactically disjoint from InstLReach (and likewise
-- |    for InstRSolve and InstRReach) because an existential variable is a
-- |    monotype.
-- | 4. InstLSolve is not syntactically disjoint from InstLArr (and likewise
-- |    for InstRSolve and InstRArr) because a function type that is a monotype
-- |    is also a polytype.
-- | 5. If the monotype is not an existential, InstLSolve and InstLReach are
-- |    disjoint because InstLReach only handles existentials. And so for
-- |    InstRSolve and InstRReach.
-- | 6. If the monotype is an existential, InstLSolve and InstLReach are
-- |    disjoint on based on whether it was introduced before or after the other
-- |    existential in the context.
-- | 7. If the function type is not a monotype, InstLSolve and InstLArr are
-- |    disjoint because InstLSolve only handles monotypes. And so for
-- |    InstRSolve and InstRArr.
-- | 8. If the function type is a monotype, InstLSolve and InstLArr are not
-- |    disjoint. However, in some cases they are disjoint based on InstLSolve's
-- |    premise (that is, whether or not the function type is well-typed with
-- |    respect to only the older items in the context, or instead is well-typed
-- |    w.r.t. the whole context). And so for InstRSolve and InstRArr.
-- | 9. If the function type is a monotype, InstLSolve and InstLArr are not
-- |    disjoint. However, in all cases where they are additionally not disjoint
-- |    based on InstLSolve's premise (that is, whether or not the function type
-- |    is well-typed with respect to only the older items in the context, or
-- |    instead is well-typed w.r.t. the whole context), they will always
-- |    produce isomorphic results, but solve encodes it more simply, and in
-- |    less steps. Thus, where they are truly not disjoint, InstLSolve can
-- |    always be preferred, as if they were disjoint. And so for InstRSolve and
-- |    InstRArr.
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
-- Is tau an existential variable? If so, is in Γ'? If both are true, apply the
-- reach rule. If either one of those is not true, apply solve, and τ must be
-- well-formed w.r.t. Γ.
instantiateL alpha poly =
  case polyIsMono poly of
    Nothing -> undefined -- TODO InstLArr
    Just tau -> do
      context <- get
      let Just (gamma', _gamma) = splitContextE alpha context -- TODO
      -- Is tau an existential variable that is in scope in gamma'?
      let whichRule = do
          beta <- monotypeIsExistential tau
          let betaIsInScope = wellFormedPolytype $ PolyTerminalType $ ExistentialTypeVar beta
          either (\_ -> Nothing) Just $ evalStateT betaIsInScope gamma'
          return beta
      case whichRule of
        -- InstLSolve
        Nothing -> undefined -- TODO
        -- InstLReach
        Just beta -> instantiateLReach alpha beta

instantiateLSolve :: (MonadState Context m, MonadError String m)
                  => Name -> Monotype -> m ()
instantiateLSolve = undefined

-- Assumes that the scoping of the existential variables has already been
-- verified. In that sense, this is not a full implementation of the rule. That
-- is because those things end up being verified in the process of determining
-- whether to run this rule.
--
-- InstLReach
instantiateLReach :: (MonadState Context m, MonadError String m)
                  => Name -> Name -> m ()
instantiateLReach = undefined

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
