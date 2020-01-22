module Main (main) where

import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.State (runStateT)
import Data.Bifunctor (first)
import qualified Data.MExpr.Parser as Parser
import qualified Data.Map as Map
import Metacore.Eval.AST
import Metacore.Eval.Eval (Interpreter, Env, eval)
import           System.IO (hFlush, stdout)
import Text.Megaparsec (parse, errorBundlePretty)

evalAndPrint :: String -> Interpreter String
evalAndPrint input = do
  mexpr <- lift $ first errorBundlePretty $ parse Parser.mexpr "repl" input
  toplevel <- case mExprToTopLevel mexpr of
                Nothing -> lift $ Left "meta/eval syntax error"
                Just x -> pure x
  x <- eval toplevel
  return $ case x of
             Nothing -> "Defined"
             Just (Right x') -> show x'
             Just (Left _) -> "<function>"

loop :: Env -> IO ()
loop env = do
  putStr "> "
  hFlush stdout
  text <- getLine
  if text == ":q"
  then putStrLn "Goodbye!"
  else do
    let (out, env') = case runStateT (evalAndPrint text) env of
                        Left e -> (e, env)
                        Right x -> x
    putStrLn out
    loop env'

main :: IO ()
main = loop Map.empty
