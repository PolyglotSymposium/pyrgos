-- | The meta/eval language's interpreter
module Metacore.Eval.Eval
  ( EvalEr, CrashTerminal, Metavalue, Env, Interpreter, eval
  ) where

import           Control.Monad.Trans.Reader (Reader, ask, runReader)
import           Control.Monad.Trans.State (State, get, modify)
import           Data.Functor ((<&>))
import           Data.MExpr (Symbol(..), MExpr(..), Emify(..))
import           Data.Map (Map)
import qualified Data.Map as Map
import           Metacore.Eval.AST

data Closure = Clo Env Symbol (Expr Terminal)

instance Emify Closure where
  emify (Clo _ arg body) = emify (Lambda arg body) -- TODO environment

type Value = Either Closure Terminal
type Env = Map Symbol Value
type Interpreter = State Env
type ExprInterpr = Reader Env

ap1 :: Emify a => Symbol -> a -> MExpr
ap1 s x = MExpr s [emify x]

quote :: Emify a => a -> MExpr
quote = ap1 $ Symbol 36 -- Q

unquote :: Emify a => a -> MExpr
unquote = ap1 $ Symbol 40 -- U

quasiquote :: Emify a => a -> MExpr
quasiquote = ap1 $ Symbol 1692 -- QQ

unableToApply :: Emify a => a -> MExpr
unableToApply = ap1 $ Symbol 1334819977440548434 -- INAPPLICABL

unboundVariable :: Symbol -> MExpr
unboundVariable x = MExpr (Symbol 224888060474 {- UNBOUND -}) [SymLit x]

data EvalEr              =
  UnableToApply Terminal |
  UnboundVariable Symbol --

instance Emify EvalEr where
  emify = \case
    UnableToApply   t -> unableToApply t
    UnboundVariable x -> unboundVariable x

data Metaterminal u n =
  Unquote  u          |
  Nonquote n          --

instance (Emify u, Emify n) => Emify (Metaterminal u n) where
  emify = \case
    Unquote  u -> unquote u
    Nonquote n -> emify n

type CrashTerminal = Metaterminal EvalEr Terminal

data Metavalue                       =
  Closure Closure                    |
  Complete Terminal                  |
  Crash (Expr CrashTerminal)         --

mapCrash :: (Expr CrashTerminal -> Expr CrashTerminal) -> Metavalue -> Metavalue
mapCrash f (Crash x) = Crash (f x)
mapCrash _ complete = complete

bindValue :: (Value -> Metavalue) -> Metavalue -> Metavalue
bindValue f (Complete x) = f (Right x)
bindValue f (Closure x) = f (Left x)
bindValue _ crash = crash

instance Emify Metavalue where
  emify = \case
    Closure clo -> emify clo
    Crash e -> quasiquote e
    Complete t -> quote t

requireClo :: Metavalue
           -> Either (Expr CrashTerminal) (Value -> Metavalue)
requireClo (Closure (Clo env param body)) =
  Right $ \arg -> runReader (evalExpr body) (Map.insert param arg env)
requireClo (Complete x) = Left $ T $ Unquote $ UnableToApply x
requireClo (Crash crash) = Left crash

requireVar :: Symbol -> ExprInterpr Metavalue
requireVar x = ask <&> \env ->
  case Map.lookup x env of
    Nothing -> Crash $ T $ Unquote $ UnboundVariable x
    Just (Left c) -> Closure c
    Just (Right v) -> Complete v

evalExpr :: Expr Terminal -> ExprInterpr Metavalue
evalExpr = \case
  Ap fExpr xExpr -> do
    fExpr' <- evalExpr fExpr
    x <- evalExpr xExpr
    return $ bindValue (helper fExpr') $ mapCrash (Ap (fmap Nonquote fExpr)) x
      where
        helper fExpr' x' =
          case requireClo fExpr' of
            Right f -> f x'
            Left m -> Crash $ Ap m (fmap Nonquote xExpr)
  Lambda arg body -> do
    env <- ask -- Is it overly greedy to close on the whole env?
    return $ Closure (Clo env arg body)
  T t -> return $ Complete t
  Var x -> requireVar x

liftReader :: Reader e a -> State e a
liftReader r = runReader r <$> get

eval :: TopLevel -> Interpreter (Maybe Metavalue)
eval = \case
  Def name value -> do
    value' <- liftReader $ evalExpr value
    case value' of
      Complete v -> do
        modify (Map.insert name (Right v))
        return Nothing
      Closure v -> do
        modify (Map.insert name (Left v))
        return Nothing
      crash -> return $ Just crash
  Eval expr ->
    Just <$> liftReader (evalExpr expr)
