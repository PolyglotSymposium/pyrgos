{-# LANGUAGE FlexibleContexts #-}
module TypeSchemes
  ( TypeScheme(..), printTypeScheme
  , freeInType
  , freeInScheme, freeInScheme', varsInScheme, varsInScheme', lastFreeSchemeVar
  , schemeSubs
  , instantiateScheme
  ) where

import Control.Monad.Except
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

foldSchemeM :: Monad m => (b -> Term -> m Term)
                       -> (b -> Name -> m (b, Name))
                       -> b -> TypeScheme -> m TypeScheme
foldSchemeM fType fForall acc (Forall name scheme) = do
  (acc', name') <- fForall acc name
  scheme' <- foldSchemeM fType fForall acc' scheme
  return $ Forall name' scheme'
foldSchemeM fType _ acc (Type term) = Type <$> fType acc term

-- | Apply substitutions to a type scheme.
-- |
-- | Much more complex than applying substitutions to a type, since a type
-- | cannot have bindings, and therefore there is no worry of scoping; the
-- | substitutions can be naively applied. In this case, however, we have to
-- | worry about the introduction of new variables in the type, and therefore
-- | fiddle with whether the substitution is applicable.
-- |
-- | Two kinds of recursion going on here: outer loop over the substitutions;
-- | inner loop on the type scheme.
schemeSubs :: MonadState Int m => Substitutions -> TypeScheme -> m TypeScheme
schemeSubs substitutions tScheme =
  -- Iterate through the substitutions
  foldSubstsM schemeSubs' tScheme substitutions where
  -- | Kick off the recursion on the type scheme itself
  schemeSubs' :: MonadState Int m => TypeScheme -> Substitution -> m TypeScheme
  schemeSubs' scheme substitution = do
    let freeInSubst = freeInType $ substTerm substitution
    let fForall = forallHelper freeInSubst substitution
    let fType = typeHelper substitution
    e <- runExceptT $ foldSchemeM fType fForall mempty scheme
    return $ either (const scheme) id e

  -- | Recurse on the type scheme itself
  forallHelper :: (MonadState Int m, MonadError () m) => [Name] -> Substitution -> Substitutions -> Name -> m (Substitutions, Name)
  forallHelper freeVars substitution rnss alpha =
    if alpha == substName substitution
    -- Short-circuit the inner loop, because if we have just introduced a
    -- binding that conflicts with the substitution, then it is bound throughout
    -- `sts`; therefore, there is no point in going further with this
    -- substitution; try the next one.
    then throwError ()
    -- The substitution doesn't conflict with the binding
    else if elem alpha freeVars
    -- If the binding is in the free variables of the substitution's type
    -- term, rename it
    then do
      -- Create a new substitution based on a new variable
      newS <- newSubst alpha
      return (sub1 newS <> rnss, substName newS)
    else return (rnss, alpha)

  typeHelper :: MonadState Int m => Substitution -> Substitutions -> Term -> m Term
  -- rnss is built up for use in this second branch
  typeHelper substitution rnss term =
    -- TODO Why `subs` twice instead of substitution composition and apply once?
    return $ subs (sub1 substitution) $ subs rnss term

instantiateScheme :: MonadState Int m => TypeScheme -> m Term
instantiateScheme (Type t) = return t
instantiateScheme (Forall alpha sigma) = do
  lastUsedSchemeVar sigma
  subst' <- newSubst alpha
  sigma' <- schemeSubs (sub1 subst') sigma
  instantiateScheme sigma'
