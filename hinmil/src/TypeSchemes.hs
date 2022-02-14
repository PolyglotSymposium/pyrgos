module TypeSchemes where

import Data.Foldable (foldl')
import Data.Char (ord, chr)
import Data.Maybe (fromJust) -- Yes, I'm a bad person

import TypeAST
import Substitutions

data TypeScheme =
  Forall String TypeScheme |
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

-- TODO clean this up later
newVar :: Int -> (Int, String)
newVar v =
  let nv = v + 1
      prime var' 0 = var'
      prime var' n = prime (var' ++ "'") (n - 1)
      primes = nv `div` 26
      var = [chr (ord 'a' + nv `mod` 26)]
  in (nv, prime var primes)

lastVar :: Int -> [Name] -> Int
lastVar nv vars =
  foldl' max nv $ varNum <$> vars

lastUsedSchemeVar :: Int -> TypeScheme -> Int
lastUsedSchemeVar nv = lastVar nv . varsInScheme

lastFreeSchemeVar :: Int -> TypeScheme -> Int
lastFreeSchemeVar nv = lastVar nv . freeInScheme

-- TODO clean up warnings
schemeSubs :: Int -> [(Term, String)] -> TypeScheme -> (Int, TypeScheme)
schemeSubs nv [] scheme = (nv, scheme)
schemeSubs nv ((tvp @ (t, v)) : tvs) scheme =
  let fvs = freeInType t
      iter nv rnss (tvp @ (t,v)) ts@(Forall sv sts) =
        if (sv == v)
        then (nv, ts)
        else if elem sv fvs
        then
          let (nv', newv) = newVar nv
              (nv'', scheme') = iter nv' (sub1 (fromJust $ subst sv (TyVar newv)) <> rnss) tvp sts
          in (nv'', Forall newv scheme')
        else
          let val (nv,scheme') = iter nv rnss tvp sts
          in (nv, Forall sv scheme')
      iter nv rnss tvp (Type term) = (nv, Type $ subs (sub1 (fromJust $ subst (snd tvp) (fst tvp))) $ subs rnss term)
      (nv, scheme') = iter nv mempty tvp scheme
  in schemeSubs nv tvs scheme'
