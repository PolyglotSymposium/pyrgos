{-# LANGUAGE FlexibleContexts #-}
module AlgorithmW
  ( Expr(..)
  , (-->)
  , InferenceFailure(..), printIFailure
  , principal
  ) where

import Assumptions
import NewVar
import Expr
import Substitutions
import TypeAST
import TypeSchemes
import Unification

import Control.Monad.State
import Control.Monad.Except
import Data.Bifunctor (first)
import Data.List.NonEmpty (NonEmpty(..))

infixr -->
(-->) :: Term -> Term -> Term
tau1 --> tau2 = TyApp "%f" (tau1 :| [tau2]) -- TODO weird choice of syntax

data InferenceFailure             =
  UndefinedVar String             |
  DoesNotUnify UnificationFailure --

printIFailure :: InferenceFailure -> String
printIFailure (UndefinedVar var) = "Undefined variable: " ++ var
printIFailure (DoesNotUnify failure) = "Failed to unify: " ++ printUFailure failure

w :: (MonadState Int m, MonadError InferenceFailure m) => Gamma -> Expr -> m (Substitutions, Term)
w gamma (Var v) = do
  scheme <- maybe (throwError $ UndefinedVar v) pure $ inContext v gamma
  term <- instantiateScheme scheme
  return (mempty, term)
w gamma (Apply e1 e2) = do
  (s1, tau1) <- w gamma e1
  s1Gamma <- assumptionSubs s1 gamma
  (s2, tau2) <- w s1Gamma e2
  let s2tau1 = subs s2 tau1
  beta <- newVar
  v <- liftEither $ first DoesNotUnify $ unify s2tau1 (tau2 --> TyVar beta)
  let vBeta = subs v (TyVar beta)
  return (v <> s2 <> s1, vBeta) -- TODO composition order?
w gamma (Lambda v e) = do
  beta <- newVar
  (s1, tau1) <- w (extend v (Type $ TyVar beta) gamma) e
  let s1beta = subs s1 (TyVar beta)
  return (s1, (s1beta --> tau1))
w gamma (Let v e1 e2) = do
  (s1, tau1) <- w gamma e1
  s1Gamma <- assumptionSubs s1 gamma
  (s2, tau2) <- w (extend v (schemeClosure s1Gamma tau1) s1Gamma) e2
  return (s2 <> s1, tau2) -- TODO composition order?

principal :: MonadError InferenceFailure m => Gamma -> Expr -> m TypeScheme
principal gamma e = evalStateT principal' $ lastFreeAssumptionVar gamma where
  principal' = do
    (s, tau) <- w gamma e
    sGamma <- assumptionSubs s gamma
    return $ schemeClosure sGamma tau
