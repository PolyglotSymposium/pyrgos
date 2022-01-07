module Lib
  ( Term(..), printTerm
  , Substitution(..), printSubst
  , Substitutions, sub1, printSubsts
  , UnificationFailure, printUFailure
  , unify
  , subs
  ) where

import Data.List.NonEmpty (NonEmpty(..), toList)
import Data.List (intersperse)
import Control.Monad (join, guard)

type Name = String

data Term = TyVar Name | TyApp Name (NonEmpty Term) deriving Eq

printTerm :: Term -> String
printTerm (TyVar name) = name
printTerm (TyApp name args) = name ++ "(" ++ join (intersperse "," $ printTerm <$> toList args) ++ ")"

data Substitution =
  -- | Substitute in the term when the name is matched.
  Subst Term Name -- "<term>/<name>"

printSubst :: Substitution -> String
printSubst (Subst term name) = printTerm term ++ "/" ++ name

newtype Substitutions = Substs [Substitution]

printSubsts :: Substitutions -> String
printSubsts (Substs xs) = "[" ++ join (intersperse ", " $ printSubst <$> xs) ++ "]"

sub1 :: Substitution -> Substitutions
sub1 s = Substs [s]

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

-- | The unification algorithm can fail three ways.
data UnificationFailure =
  -- Arity mismatch between two type applications
  ArityMismatch         |
  -- Expansion of substitutions would result in nontermination
  CircularOccurence     |
  -- We have no higher-order unification here so we can only unify type
  -- functions that are literally the same, to wit, the same name.
  TypeFunctionMismatch  --

printUFailure :: UnificationFailure -> String
printUFailure ArityMismatch = "arity mismatch"
printUFailure CircularOccurence = "circular occurrence"
printUFailure TypeFunctionMismatch = "type function mismatch"

-- | Check for circular occurrence. Circularity would result in nontermination
-- | of our typechecker and unsoundness of our type system.
occurs :: Name -> Term -> Bool
occurs name (TyVar vname) = name == vname
occurs name (TyApp _ args) = any (occurs name) args

unify :: Term -> Term -> Either UnificationFailure Substitutions
unify t1@(TyVar v1) (TyVar v2) =
  Right $ Substs $ guard (v1 == v2) *> [Subst t1 v2]
unify (TyVar v) t2@(TyApp _ _) =
  if occurs v t2 then Left CircularOccurence else Right $ Substs [(Subst t2 v)]
unify t1@(TyApp _ _) (TyVar v) =
  if occurs v t1 then Left CircularOccurence else Right $ Substs [(Subst t1 v)]
unify (TyApp name1 args1) (TyApp name2 args2) =
  if (name1 == name2)
  then unify_args (Substs []) args1 args2
  else Left TypeFunctionMismatch

unify_args :: Substitutions -> NonEmpty Term -> NonEmpty Term -> Either UnificationFailure Substitutions
unify_args substs (arg1 :| arg1s) (arg2 :| arg2s) = do
  substitutions' <- unify (subs substs arg1) (subs substs arg2)
  case (arg1s, arg2s) of
    ([], []) ->
      let Substs ss = substs <> substitutions'
      in Right $ Substs $ reverse ss
    (arg1' : arg1s', arg2' : arg2s') ->
      let ss = substs <> substitutions'
          args1 = arg1' :| arg1s'
          args2 = arg2' :| arg2s'
      in unify_args ss args1 args2
    (_, _)  -> Left ArityMismatch
