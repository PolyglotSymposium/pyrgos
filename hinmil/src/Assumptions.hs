module Assumptions where

import qualified Data.Map as Map
import Data.Foldable (foldl')
import Data.Map (Map)
import Control.Monad.State

import TypeAST
import TypeSchemes
import Substitutions

type Gamma = Map Name TypeScheme

extend :: Name -> TypeScheme -> Gamma -> Gamma
extend = Map.insert

assumptionVarsBy :: ([Name] -> [Name] -> TypeScheme -> [Name]) -> Gamma -> [Name]
assumptionVarsBy getVarsFromScheme =
  Map.foldr (\x f -> f ++ getVarsFromScheme f [] x) []

-- | Run through all the type schemes in the environment (values in the map) and
-- | find variables that are free in those schemes. Because these variables are
-- | free they are unification variables (TODO right?).
freeAssumptionVars :: Gamma -> [Name]
freeAssumptionVars = assumptionVarsBy freeInScheme'

assumptionVars :: Gamma -> [Name]
assumptionVars = assumptionVarsBy varsInScheme'

lastFreeAssumptionVar :: Gamma -> Int
lastFreeAssumptionVar =
  let hack = -1 -- TODO do we really mean -1 or do we mean Nothing :: Maybe Int?
  in Map.foldr (flip lastFreeSchemeVar) hack

-- | Apply the substitutions to every schema in the typing environment.
assumptionSubs :: Substitutions -> Gamma -> State Int Gamma
assumptionSubs = traverse . schemeSubs

schemeClosure :: Gamma -> Term -> TypeScheme
schemeClosure gamma tau =
  let favs = freeAssumptionVars gamma
      bindIfAbsolutelyFree (binders, boundVars) v =
        -- We do not want to clobber any unification variables. Therefore if
        -- this variable is a free assumption variable, skip it. Likewise, if we
        -- have just bound it, we don't need to bind it again, so skip it.
        let (bind, addBound) = if elem v favs || elem v boundVars
                               then (id, id)
                               else (Forall v, (v :))
        in (bind . binders, addBound boundVars)
      binders' = fst $ foldl' bindIfAbsolutelyFree (id, []) $ freeInType tau
  in binders' $ Type tau
