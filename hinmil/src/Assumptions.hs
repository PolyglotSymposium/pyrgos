module Assumptions where

import qualified Data.Map as Map
import Data.Map (Map)

import TypeAST
import TypeSchemes

type Gamma = Map Name TypeScheme

assumptionVarsBy :: ([Name] -> [String] -> TypeScheme -> [Name]) -> Gamma -> [Name]
assumptionVarsBy getVarsFromScheme =
  Map.foldr (\ts f -> f ++ getVarsFromScheme f [] ts) []

freeAssumptionVars :: Gamma -> [Name]
freeAssumptionVars = assumptionVarsBy freeInScheme'

assumptionVars :: Gamma -> [Name]
assumptionVars = assumptionVarsBy varsInScheme'
