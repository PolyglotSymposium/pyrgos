module TypeSchemes where

import Data.Char (ord, chr)
import Data.Foldable (foldl')
import Data.Maybe (fromJust) -- Yes, I'm a bad person
import Control.Monad.State

import TypeAST
import Substitutions

data TypeScheme =
  Forall Name TypeScheme |
  Type Term

freeInType' :: [Name] -> [Name] -> Term -> [Name]
freeInType' free bound (TyVar var) =
  if elem var bound
  then free
  else var : free
freeInType' free bound (TyApp _ args) =
  foldl' (\r t -> freeInType' r bound t) free args

freeAndBoundInScheme' :: [Name] -> [String] -> TypeScheme -> ([Name], [Name])
freeAndBoundInScheme' free bound (Forall var scheme) =
  freeAndBoundInScheme' free (var : bound) scheme
freeAndBoundInScheme' free bound (Type typ) =
  (freeInType' free bound typ, bound)

freeAndBoundInScheme :: TypeScheme -> ([Name], [Name])
freeAndBoundInScheme = freeAndBoundInScheme' [] []

freeInScheme :: TypeScheme -> [Name]
freeInScheme = fst . freeAndBoundInScheme

varsInScheme :: TypeScheme -> [Name]
varsInScheme scheme =
  let (free, bound) = freeAndBoundInScheme scheme
  in free ++ bound

freeInType :: Term -> [Name]
freeInType = freeInType' [] []

-- TODO clean this up later
varNum :: String -> Int
varNum "" = -1 -- hack???
varNum (hh : tt) =
  let letter = ord hh - ord 'a'
      primes r [] = r
      primes r (h : t) =
        if h == '\039'
        then primes (r+26) t
        else -1
  in if letter >= 0 && letter <= 25
  then primes letter tt
  else -1

lastVar :: Int -> [Name] -> Int
lastVar nv vars =
  foldl' max nv $ varNum <$> vars

lastUsedSchemeVar :: Int -> TypeScheme -> Int
lastUsedSchemeVar nv = lastVar nv . varsInScheme

lastFreeSchemeVar :: Int -> TypeScheme -> Int
lastFreeSchemeVar nv = lastVar nv . freeInScheme

foldTypeScheme :: (Name -> a) -> (Term -> a) -> (a -> a -> a) -> TypeScheme -> a
foldTypeScheme _ g _ (Type term) = g term
foldTypeScheme f g h (Forall name scheme) = h (f name) $ foldTypeScheme f g h scheme

schemeSubs :: Substitutions -> TypeScheme -> State Int TypeScheme
schemeSubs substitutions tScheme =
  foldSubstsM schemeSubs' tScheme substitutions where
  schemeSubs' :: TypeScheme -> Substitution -> State Int TypeScheme
  schemeSubs' scheme substitution =
    iter (freeInType $ substTerm substitution) mempty substitution scheme
  iter :: [Name] -> Substitutions -> Substitution -> TypeScheme -> State Int TypeScheme
  iter fvs rnss substitution ts@(Forall alpha sts) =
    if alpha == substName substitution
    then return ts
    else if elem alpha fvs
    then do
      newS <- newSubst alpha
      ts' <- iter fvs (sub1 newS <> rnss) substitution sts
      return $ Forall (substName newS) ts'
    else Forall alpha <$> iter fvs rnss substitution sts
  iter _ rnss substitution (Type term) =
    return $ Type $ subs (sub1 substitution) $ subs rnss term
