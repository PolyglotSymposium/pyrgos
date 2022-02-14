module TypeSchemes where

import TypeAST
import Data.Foldable (foldl')

data TypeScheme =
  Forall String TypeScheme |
  Type Term

freeInType :: [Name] -> [Name] -> Term -> [Name]
freeInType free bound (TyVar var) =
  if elem var bound
  then free
  else var : free
freeInType free bound (TyApp _ args) =
  foldl' (\r t -> freeInType r bound t) free args

freeAndBoundInScheme :: [Name] -> [String] -> TypeScheme -> ([Name], [Name])
freeAndBoundInScheme free bound (Forall var scheme) =
  freeAndBoundInScheme free (var : bound) scheme
freeAndBoundInScheme free bound (Type typ) =
  (freeInType free bound typ, bound)

tyVars :: Term -> [Name]
tyVars = freeInType [] []
