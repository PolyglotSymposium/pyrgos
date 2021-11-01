module Lib where

import Data.List.NonEmpty (NonEmpty(..))

type Name = String
type Substitutions = [(Term, Name)]

data Term = TyVar Name | TyApp Name (NonEmpty Term)

subs :: Substitutions -> Term -> Term
subs [] term = term
subs ((t1, v1) : ss) term@(TyVar name) = if name == v1 then t1 else subs ss term
subs s (TyApp name args) = TyApp name $ fmap (subs s) args

compose :: Substitutions -> Substitutions -> Substitutions
compose [] s1 = s1
compose (s : ss) s1 = compose ss (s : (iter [] s s1)) where
  iter r _ [] = reverse r
  iter r _ ((t1, v1) : _) = iter (((subs [s] t1), v1) : r) s ss

data UnificationFailure = ArityMismatch | CircularOccurence | ConstError -- ?

unify :: Term -> Term -> Either UnificationFailure Substitutions
unify t1 t2 = iter [] t1 t2 where
  iter :: Substitutions -> Term -> Term -> Either UnificationFailure Substitutions
  iter r (TyVar v1) (TyVar v2) = Right $ if (v1 == v2) then [] else ((t1, v2) : r)
  iter r (TyVar v) (TyApp _ _) = if occurs v t2 then Left CircularOccurence else Right ((t2, v) : r)
  iter r (TyApp _ _) (TyVar v) = if occurs v t1 then Left CircularOccurence else Right ((t1, v) : r)
  iter r (TyApp name1 args1) (TyApp name2 args2) =
    if (name1 == name2)
    then unify_args r args1 args2
    else Left ConstError

  occurs :: Name -> Term -> Bool
  occurs v (TyApp name ((TyVar vn) :| [])) = vn == v
  occurs v (TyApp name ((TyVar vn) :| (t:tt))) = if vn==v then True else occurs v (TyApp name (t :| tt))
  occurs v (TyApp name (s :| [])) = occurs v s
  occurs v (TyApp name (s :| (t:tt))) = occurs v s || occurs v (TyApp name (t :| tt))
  occurs v (TyVar vn) = vn == v

  unify_args :: Substitutions -> NonEmpty Term -> NonEmpty Term -> Either UnificationFailure Substitutions
  unify_args r [] [] = Right $ reverse r
  unify_args _ [] _ = Left ArityMismatch
  unify_args _ _ [] = Left ArityMismatch
  unify_args r (t1 : t1s) (t2 : t2s) = unify_args (compose (iter [] (subs r t1) (subs r t2)) r) t1s t2s
