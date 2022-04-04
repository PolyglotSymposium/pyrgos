module AlgorithmW where

import Assumptions
import NewVar
import Substitutions
import TypeAST
import TypeSchemes
import Unification

import Control.Monad.State
import Control.Monad.Except
import Data.Bifunctor (first)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map as Map

data Expr             =
  Var Name            |
  Apply Expr Expr     |
  Lambda String Expr  |
  Let Name Expr Expr  --

infixr -->
(-->) :: Term -> Term -> Term
tau1 --> tau2 = TyApp "%f" (tau1 :| [tau2])

data InferenceFailure             =
  UndefinedVar String             |
  DoesNotUnify UnificationFailure --

w :: Gamma -> Expr -> ExceptT InferenceFailure (State Int) (Substitutions, Term)
w gamma (Var v) = do
  scheme <- maybe (throwError $ UndefinedVar v) pure $ Map.lookup v gamma
  term <- lift $ instantiateScheme scheme
  return (mempty, term)
w gamma (Apply e1 e2) = do
  (s1, tau1) <- w gamma e1
  s1Gamma <- lift $ assumptionSubs s1 gamma
  (s2, tau2) <- w s1Gamma e2
  let s2tau1 = subs s2 tau1
  beta <- lift newVar
  v <- liftEither $ first DoesNotUnify $ unify s2tau1 (tau2 --> TyVar beta)
  let vBeta = subs v (TyVar beta)
  return (v <> s2 <> s1, vBeta)
w gamma (Lambda v e) = do
  beta <- lift newVar
  (s1, tau1) <- w (extend v (Type $ TyVar beta) gamma) e
  let s1beta = subs s1 (TyVar beta)
  return (s1, (s1beta --> tau1))
w gamma (Let v e1 e2) = do
  (s1, tau1) <- w gamma e1
  s1Gamma <- lift $ assumptionSubs s1 gamma
  (s2, tau2) <- w (extend v (schemeClosure s1Gamma tau1) s1Gamma) e2
  return (s2 <> s1, tau2)
