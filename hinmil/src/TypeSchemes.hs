{-# LANGUAGE FlexibleContexts #-}
module TypeSchemes
  ( TypeScheme(..), printTypeScheme
  , freeInType
  , freeInScheme, freeInScheme', varsInScheme, varsInScheme', lastFreeSchemeVar
  , schemeSubs
  , instantiateScheme
  ) where

import Control.Monad.State
import Data.Char (ord)
import Data.Foldable (foldl')

import TypeAST
import Substitutions

data TypeScheme =
  Forall Name TypeScheme |
  Type Term

printTypeScheme :: TypeScheme -> String
printTypeScheme (Type term) = printTerm term
printTypeScheme (Forall name scheme) = "forall " ++ name ++ ". " ++ printTypeScheme scheme

freeInType' :: [Name] -> [Name] -> Term -> [Name]
freeInType' free _ (TyLit _) = free
freeInType' free bound (TyVar var) =
  if elem var bound
  then free
  else var : free
freeInType' free bound (TyApp _ args) =
  foldl' (\r t -> freeInType' r bound t) free args

freeAndBoundInScheme' :: [Name] -> [Name] -> TypeScheme -> ([Name], [Name])
freeAndBoundInScheme' free bound (Forall var scheme) =
  freeAndBoundInScheme' free (var : bound) scheme
freeAndBoundInScheme' free bound (Type typ) =
  (freeInType' free bound typ, bound)

freeAndBoundInScheme :: TypeScheme -> ([Name], [Name])
freeAndBoundInScheme = freeAndBoundInScheme' [] []

freeInScheme' :: [Name] -> [Name] -> TypeScheme -> [Name]
freeInScheme' free bound = fst . freeAndBoundInScheme' free bound

freeInScheme :: TypeScheme -> [Name]
freeInScheme = fst . freeAndBoundInScheme

varsInScheme' :: [Name] -> [Name] -> TypeScheme -> [Name]
varsInScheme' free bound scheme =
  let (free', bound') = freeAndBoundInScheme' free bound scheme
  in free' ++ bound'

varsInScheme :: TypeScheme -> [Name]
varsInScheme scheme =
  let (free, bound) = freeAndBoundInScheme scheme
  in free ++ bound

freeInType :: Term -> [Name]
freeInType = freeInType' [] []

-- TODO clean this up later
varNum :: String -> Int
varNum "" = -1 -- TODO hack???
varNum (hh : tt) =
  let letter = ord hh - ord 'a'
      primes r [] = r
      primes r (h : t) =
        if h == '\039'
        then primes (r+26) t
        else -1 -- TODO augh
  in if letter >= 0 && letter <= 25
  then primes letter tt
  else -1 -- TODO halp

lastVar :: MonadState Int m => [Name] -> m ()
lastVar vars = modify (\nv -> foldl' max nv $ varNum <$> vars)

lastUsedSchemeVar :: MonadState Int m => TypeScheme -> m ()
lastUsedSchemeVar = lastVar . varsInScheme

lastFreeSchemeVar :: TypeScheme -> State Int ()
lastFreeSchemeVar = lastVar . freeInScheme

-- | Two kinds of recursion going on here: outer loop over the substitutions;
-- | inner loop on the type scheme.
schemeSubs :: MonadState Int m => Substitutions -> TypeScheme -> m TypeScheme
schemeSubs substitutions tScheme =
  -- Iterate through the substitutions
  foldSubstsM schemeSubs' tScheme substitutions where
  -- | Kick off the recursion on the type scheme itself
  schemeSubs' :: MonadState Int m => TypeScheme -> Substitution -> m TypeScheme
  schemeSubs' scheme substitution =
    iter (freeInType $ substTerm substitution) mempty substitution scheme
  -- | Recurse on the type scheme itself
  iter :: MonadState Int m => [Name] -> Substitutions -> Substitution -> TypeScheme -> m TypeScheme
  iter fvs rnss substitution ts@(Forall alpha sts) =
    if alpha == substName substitution
    then return ts -- short-circuit the inner loop (TODO why?)
    else do
      (rnss', alpha') <-
        -- If the binding is in the free variables, rename it
        if elem alpha fvs
        then do
          -- Create a new substitution based on a new variable
          newS <- newSubst alpha
          return (sub1 newS <> rnss, substName newS)
        else return (rnss, alpha)
      -- Recurse over other type scheme bindings
      ts' <- iter fvs rnss' substitution sts
      return $ Forall alpha' ts'
  iter _ rnss substitution (Type term) =
    -- TODO Why `subs` twice instead of substitution composition and apply once?
    return $ Type $ subs (sub1 substitution) $ subs rnss term

instantiateScheme :: MonadState Int m => TypeScheme -> m Term
instantiateScheme (Type tau') = return tau'
instantiateScheme (Forall alpha sigma) = do
  lastUsedSchemeVar sigma
  subst' <- newSubst alpha
  sigma' <- schemeSubs (sub1 subst') sigma
  instantiateScheme sigma'
