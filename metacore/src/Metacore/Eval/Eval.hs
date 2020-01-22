-- | The meta/eval language's interpreter
module Metacore.Eval.Eval (Env, Interpreter, eval) where

import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Reader (ReaderT, local, ask, runReaderT)
import           Control.Monad.Trans.State (StateT, get, modify)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Word (Word64)
import           Metacore.Eval.AST

type Value = Either Fun Terminal

type Env = Map Word64 Value

type Interpreter = StateT Env (Either String)
type ExprInterpr = ReaderT Env (Either String)

requireFun :: Value -> ExprInterpr Fun
requireFun (Right x) =
  lift $ Left $ "Expected a function, got: " ++ show x
requireFun (Left f) = return f

requireVar :: Word64 -> ExprInterpr Value
requireVar x = do
  env <- ask
  case Map.lookup x env of
    Nothing -> lift $ Left $ "Unbound name: " ++ show x
    Just v -> return v

evalExpr :: Expr -> ExprInterpr Value
evalExpr = \case
  Ap f x -> do
    f' <- evalExpr f
    x' <- evalExpr x
    Fun arg body <- requireFun f'
    local (Map.insert arg x') (evalExpr body)
  T t -> return $ Right t
  Var x -> requireVar x

liftReader :: Monad m => ReaderT e m a -> StateT e m a
liftReader r = do
  env <- get
  let x = runReaderT r env
  lift x

eval :: TopLevel -> Interpreter (Maybe Value)
eval = \case
  DefVar name value -> do
    value' <- liftReader $ evalExpr value
    modify (Map.insert name value')
    return Nothing
  DefFun name fun -> do
    modify (Map.insert name $ Left fun)
    return Nothing
  Eval expr ->
    Just <$> liftReader (evalExpr expr)
