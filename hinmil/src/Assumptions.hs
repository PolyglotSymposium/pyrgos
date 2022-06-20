{-# LANGUAGE FlexibleContexts #-}
module Assumptions
  ( Gamma, emptyGamma, extend, inContext
  , assumptionSubs
  , closeScheme
  , lastFreeAssumptionVar
  ) where

import Control.Monad.State
import Data.Foldable (foldl')
import Data.Map (Map)
import qualified Data.Map as Map

import TypeAST
import TypeSchemes
import Substitutions

newtype Gamma = Gamma (Map Name TypeScheme)

emptyGamma :: Gamma
emptyGamma = Gamma Map.empty

extend :: Name -> TypeScheme -> Gamma -> Gamma
extend name scheme (Gamma gamma) = Gamma $ Map.insert name scheme gamma

inContext :: Name -> Gamma -> Maybe TypeScheme
inContext name (Gamma gamma) = Map.lookup name gamma

assumptionVarsBy :: ([Name] -> [Name] -> TypeScheme -> [Name]) -> Gamma -> [Name]
assumptionVarsBy getVarsFromScheme (Gamma gamma) =
  Map.foldr (\x f -> f ++ getVarsFromScheme f [] x) [] gamma

-- | Run through all the type schemes in the environment (values in the map) and
-- | find variables that are free in those schemes. Because these variables are
-- | free they are unification variables (TODO right?).
freeAssumptionVars :: Gamma -> [Name]
freeAssumptionVars = assumptionVarsBy freeInScheme'

lastFreeAssumptionVar :: Gamma -> Int
lastFreeAssumptionVar (Gamma gamma) =
  let hack = -1 -- TODO do we really mean -1 or do we mean Nothing :: Maybe Int?
  in Map.foldr (\x y -> execState (lastFreeSchemeVar x) y) hack gamma

-- | Apply the substitutions to every schema in the typing environment.
assumptionSubs :: MonadState Int m => Substitutions -> Gamma -> m Gamma
assumptionSubs substitutions (Gamma gamma) = Gamma <$> traverse (schemeSubs substitutions) gamma

closeScheme :: Gamma -> Term -> TypeScheme
closeScheme gamma tau =
  let favs = freeAssumptionVars gamma
      bindIfAbsolutelyFree (binders, boundVars) v =
        -- We do not want to clobber any variables that are free in the
        -- assumptions. Therefore if this variable is a free assumption
        -- variable, skip it. Likewise, if we have just bound it, we don't need
        -- to bind it again, so skip it.
        let (bind, addBound) = if elem v favs || elem v boundVars
                               then (id, id)
                               else (Forall v, (v :))
        in (binders . bind, addBound boundVars)
      binders' = fst $ foldl' bindIfAbsolutelyFree (id, []) $ freeInType tau
  in binders' $ Type tau
