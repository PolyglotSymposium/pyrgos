{-# LANGUAGE TypeApplications #-}
module Unification
  (UnificationFailure, printUFailure
  , unify
  ) where

import Control.Monad (foldM)
import Data.Either.Combinators (maybeToRight)
import Data.Foldable (fold)
import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty.Exact
import Data.Function ((&))

import TypeAST
import Substitutions

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

mustNotOccur :: Name -> Term -> Either UnificationFailure Substitutions
mustNotOccur name term =
  sub1 <$> (maybeToRight CircularOccurence $ subst name term)

typeFunctionsMatch :: Name -> Name -> Either UnificationFailure ()
typeFunctionsMatch name1 name2 =
  if (name1 == name2)
  then Right ()
  else Left TypeFunctionMismatch

arityMatch :: NonEmpty Term -> NonEmpty Term -> Either UnificationFailure (NonEmpty (Term, Term))
arityMatch args1 args2 = maybeToRight ArityMismatch $ zipExactNEL args1 args2

typeVarsMismatch :: Name -> Name -> Substitutions
typeVarsMismatch name1 name2 =
  subst name2 (TyVar name1)
  <&> sub1
  & fold

unify :: Term -> Term -> Either UnificationFailure Substitutions
unify (TyVar name1) (TyVar name2) = return $ typeVarsMismatch name1 name2
unify (TyVar name) term@(TyApp _ _) = mustNotOccur name term
unify term@(TyApp _ _) (TyVar name) = mustNotOccur name term
unify (TyApp name1 args1) (TyApp name2 args2) = do
  () <- typeFunctionsMatch name1 name2
  args <- arityMatch args1 args2
  unifyArgs args

unifyArgs :: NonEmpty (Term, Term) -> Either UnificationFailure Substitutions
unifyArgs = foldM unifyArg mempty

unifyArg :: Substitutions -> (Term, Term) -> Either UnificationFailure Substitutions
unifyArg substs (arg1, arg2) =
  -- Order of substitutions composition critical
  (substs <>) <$> unify (subs substs arg1) (subs substs arg2)
