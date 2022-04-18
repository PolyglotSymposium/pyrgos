{-# LANGUAGE FlexibleContexts #-}
module Substitutions
  ( Substitution, subst, newSubst, substName, substTerm, printSubst
  , Substitutions, sub1, printSubsts, foldSubstsM
  , subs
  ) where

import Control.Monad.State
import Data.Functor (($>))
import Data.List (intersperse)

import TypeAST
import NewVar

data Substitution =
  -- | Substitute in the term when the name is matched.
  Subst Term Name -- "<term>/<name>"
  deriving (Eq, Show)

printSubst :: Substitution -> String
printSubst (Subst term name) = printTerm term ++ "/" ++ name

substName :: Substitution -> Name
substName (Subst _ x) = x

substTerm :: Substitution -> Term
substTerm (Subst x _) = x

newtype Substitutions = Substs [Substitution] deriving (Eq, Show)

printSubsts :: Substitutions -> String
printSubsts (Substs xs) = "[" ++ join (intersperse ", " $ printSubst <$> xs) ++ "]"

sub1 :: Substitution -> Substitutions
sub1 s = Substs [s]

foldSubstsM :: Monad m => (a -> Substitution -> m a) -> a -> Substitutions -> m a
foldSubstsM f x (Substs ss) = foldM f x ss

-- | Substitutions closer to the head get precedence over ones closer to the
-- | tail. Substitutions are pre-expanded as they are composed so we do not need
-- | to recursively apply them here.
subs :: Substitutions -> Term -> Term
-- If we have no substitutions, return the same term.
subs (Substs []) term = term
-- If we have hit a leaf of our type AST, to wit, a type variable, then iterate
-- through the substitutions, looking for a match on its name. If found,
-- substitute in the corresponding term, and short-circuit.
subs (Substs (Subst t1 v1 : ss)) term@(TyVar name) =
  if name == v1 then t1 else subs (Substs ss) term
-- If we have hit a type application, apply all the substitutions to all the
-- arguments of the type operator.
subs s (TyApp name args) = TyApp name $ fmap (subs s) args

-- | Compose two substitutions in order to produce one substitution (as opposed
-- | to composing two sets of substitutions in order to produce a set of
-- | substitutions) is a way of making the left substitution more concrete
-- | (assuming that the two substitutions have any relationship). The
-- | substitution on the right is absorbed into it. The need to still have it
-- | stand alone is handled in the composition of sets of substitutions rather
-- | than here.
-- |
-- | For composition of sets of substitutions, see the Substitutions Semigroup
-- | below.
instance Semigroup Substitution where
  -- Substitutions on the right are applied to the terms in the substitutions on
  -- the left. Do not confuse right and left here with being nearer to the head
  -- or tail in the whole set of substitutions.
  (Subst tL vL) <> sR = Subst (subs (sub1 sR) tL) vL

-- | Composition of sets of substitutions
instance Semigroup Substitutions where
  -- Substitutions from the right need to be applied to substitutions from the
  -- left before being concatenated with the them.
  -- Substitutions from the right become leftmost (highest-priority).
  Substs seed <> Substs foldee = Substs $ foldr oneSubst seed foldee where
    -- We take the next highest-precedence substitution from the right, make it
    -- our new highest-precedence substitution (with `:`), and apply it to all
    -- of the existing lower-precedence substitutions (with `fmap` and `<>`).
    oneSubst :: Substitution -> [Substitution] -> [Substitution]
    oneSubst s ss = s : fmap (<> s) ss

instance Monoid Substitutions where
  mempty = Substs []

-- | Check for circular occurrence. Circularity would result in nontermination
-- | of our typechecker and unsoundness of our type system.
occurs :: Name -> Term -> Bool
occurs name (TyVar vname) = name == vname
occurs name (TyApp _ args) = any (occurs name) args

-- | Smart constructor of substitution that checks circular occurrence
subst :: Name -> Term -> Maybe Substitution
subst name term =
  guard (not $ occurs name term) $> Subst term name

-- | Occurs check not needed since the variable is new. It "cannot" occur
-- | therefore in the term.
newSubst :: MonadState Int m => Name -> m Substitution
newSubst name = do
  x <- newVar
  return $ Subst (TyVar x) name
