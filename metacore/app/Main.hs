module Main (main) where

import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.State (StateT, mapStateT, runStateT)
import           Data.Bifunctor (first)
import           Data.Functor.Identity (Identity(..))
import           Data.MExpr (emify, deemify)
import qualified Data.MExpr.Parser as Parser
import qualified Data.Map as Map
import           Data.Foldable (traverse_)
import           Metacore.Eval.Eval (Env, Metavalue, eval)
import           System.IO (hFlush, stdout)
import           Text.Megaparsec (parse, errorBundlePretty)

type Rep = StateT Env (Either String)

evalAndPrint :: String -> Rep (Maybe Metavalue)
evalAndPrint input = do
  mexpr <- lift $ first errorBundlePretty $ parse Parser.mexpr "repl" input
  toplevel <- case deemify mexpr of
                Nothing -> lift $ Left "meta/eval syntax error"
                Just x -> pure x
  x <- mapStateT (\(Identity x) -> Right x) (eval toplevel)
  lift $ traverse (Left . show . emify) x

loop :: Env -> IO ()
loop env = do
  putStr "> "
  hFlush stdout
  text <- getLine
  if text == ":q"
  then putStrLn "Goodbye!"
  else do
    let (out, env') = case runStateT (evalAndPrint text) env of
                        Left e -> (Just e, env)
                        Right (x, env) -> (fmap (show . emify) x, env)
    traverse_ putStrLn out
    loop env'

main :: IO ()
main = loop Map.empty
