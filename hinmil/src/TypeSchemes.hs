module TypeSchemes where

import TypeAST
import Data.List.NonEmpty (NonEmpty(..))

data TypeScheme =
  Forall String TypeScheme |
  Type Term

-- TODO NonEmpty probably becoming a bad idea
freeAndBoundInType :: [Name] -> [Name] -> Term -> ([Name], [Name])
freeAndBoundInType free bound (TyVar v) =
  if elem v bound
  then (free, bound)
  else (v : free, bound)
freeAndBoundInType free bound (TyApp _ args) =
  let iter r (t :| []) = fst $ freeAndBoundInType r bound t
      iter r (t :| t' : ts) =
        let (free', _) = freeAndBoundInType r bound t
        in iter free' (t' :| ts)
      fvs = iter free args
  in (fvs, bound)

freeAndBoundInScheme :: [Name] -> [String] -> TypeScheme -> ([Name], [Name])
freeAndBoundInScheme f b (Forall v s) = freeAndBoundInScheme f (v : b) s
freeAndBoundInScheme f b (Type t) = freeAndBoundInType f b t

tyVars :: Term -> [Name]
tyVars t =
  let (free, bound) = freeAndBoundInType [] [] t
  in free ++ bound
