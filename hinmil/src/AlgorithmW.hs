module AlgorithmW where

import Assumptions
import NewVar
import Substitutions
import TypeAST
import TypeSchemes
import Unification

import Control.Monad.State
import Data.Either (fromRight) -- TODO kill
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map as Map
import Data.Maybe (fromJust) -- TODO kill

data Expr             =
  Var Name            |
  Comb Expr Expr      | -- TODO rename Comb? Apply?
  Abs String Expr     | -- TODO rename Abs? Lam?
  Let Name Expr Expr  --

infixr -->
(-->) :: Term -> Term -> Term
tau1 --> tau2 = TyApp "%f" (tau1 :| [tau2])

w :: Gamma -> Expr -> State Int (Substitutions, Term)
w gamma (Var v) = do
  term <- instantiateScheme $ fromJust $ Map.lookup v gamma -- TODO kill fromJust
  return (mempty, term)
w gamma (Comb e1 e2) = do
  (s1, tau1) <- w gamma e1
  s1Gamma <- assumptionSubs s1 gamma
  (s2, tau2) <- w s1Gamma e2
  let s2tau1 = subs s2 tau1
  beta <- newVar
  let v = fromRight undefined $ unify s2tau1 (tau2 --> TyVar beta) -- TODO kill fromRight
  let vBeta = subs v (TyVar beta)
  return (v <> s2 <> s1, vBeta)
w gamma (Abs v e) = do
  beta <- newVar
  (s1, tau1) <- w (extend v (Type $ TyVar beta) gamma) e
  let s1beta = subs s1 (TyVar beta)
  return (s1, (s1beta --> tau1))
w gamma (Let v e1 e2) = do
  (s1, tau1) <- w gamma e1
  s1Gamma <- assumptionSubs s1 gamma
  (s2, tau2) <- w (extend v (schemeClosure s1Gamma tau1) s1Gamma) e2
  return (s2 <> s1, tau2)
