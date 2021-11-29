module Lib where

import Data.List.NonEmpty (NonEmpty(..), toList)
import Data.List (intersperse)
import Control.Monad (join)

type Name = String

data Term = TyVar Name | TyApp Name (NonEmpty Term)

printTerm :: Term -> String
printTerm (TyVar name) = name
printTerm (TyApp name args) = name ++ "(" ++ join (intersperse "," $ printTerm <$> toList args) ++ ")"

data Substitution =
  -- | Substitute in the term when the name is matched.
  Subst Term Name

printSubst :: Substitution -> String
printSubst (Subst term name) = printTerm term ++ "/" ++ name

newtype Substitutions = Substs [Substitution]

printSubsts :: Substitutions -> String
printSubsts (Substs xs) = "[" ++ join (intersperse ", " $ printSubst <$> xs) ++ "]"

sub1 :: Substitution -> Substitutions
sub1 s = Substs [s]

-- | Substitutions closer to the head get precedence over ones closer to the
-- | tail.
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

instance Semigroup Substitution where
  -- Substitutions on the right are applied to the terms in the substitutions on
  -- the left.
  (Subst t2 v2) <> s1 = Subst (subs (sub1 s1) t2) v2

instance Semigroup Substitutions where
  -- Substitutions from the right need to be applied to substitutions from the
  -- left before being concatenated with the them.
  -- Substitutions from the right become leftmost (highest-priority).
  Substs seed <> Substs foldee = Substs $ foldr oneSubst seed foldee where
    -- We take the next highest-precedence substitution from the right, make it
    -- our new highest-precedence substitution, and apply it to all of the
    -- existing lower-precedence substitutions.
    oneSubst :: Substitution -> [Substitution] -> [Substitution]
    oneSubst s ss = s : fmap (<> s) ss

data UnificationFailure = ArityMismatch | CircularOccurence | TypeFunctionMismatch

occurs :: Name -> Term -> Bool
occurs v (TyApp _ ((TyVar vn) :| [])) = vn == v
occurs v (TyApp name ((TyVar vn) :| (t:tt))) = if vn == v then True else occurs v (TyApp name (t :| tt))
occurs v (TyApp _ (s :| [])) = occurs v s
occurs v (TyApp name (s :| (t:tt))) = occurs v s || occurs v (TyApp name (t :| tt))
occurs v (TyVar vn) = vn == v

unify :: Term -> Term -> Either UnificationFailure Substitutions
unify t1 t2 = iter (Substs []) t1 t2 where
  iter :: Substitutions -> Term -> Term -> Either UnificationFailure Substitutions
  iter (Substs r) (TyVar v1) (TyVar v2) = Right $ Substs $ if (v1 == v2) then [] else (Subst t1 v2 : r)
  iter (Substs r) (TyVar v) (TyApp _ _) =
    if occurs v t2 then Left CircularOccurence else Right $ Substs (Subst t2 v : r)
  iter (Substs r) (TyApp _ _) (TyVar v) =
    if occurs v t1 then Left CircularOccurence else Right $ Substs (Subst t1 v : r)
  iter r (TyApp name1 args1) (TyApp name2 args2) =
    if (name1 == name2)
    then unify_args r args1 args2
    else Left TypeFunctionMismatch

  unify_args :: Substitutions -> NonEmpty Term -> NonEmpty Term -> Either UnificationFailure Substitutions
  unify_args substs (arg1 :| arg1s) (arg2 :| arg2s) = do
    substitutions' <- iter (Substs []) (subs substs arg1) (subs substs arg2)
    case (arg1s, arg2s) of
      ([], []) ->
        let Substs ss = substitutions'
        in Right $ Substs $ reverse ss
      (arg1' : arg1s', arg2' : arg2s') ->
        let ss = substitutions' <> substs
            args1 = arg1' :| arg1s'
            args2 = arg2' :| arg2s'
        in unify_args ss args1 args2
      (_, _)  -> Left ArityMismatch
