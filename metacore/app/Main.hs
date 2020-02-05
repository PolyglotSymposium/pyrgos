{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.State (StateT, mapStateT, runStateT)
import           Data.Bifunctor (first)
import           Data.Foldable (traverse_)
import           Data.Functor.Identity (Identity(..))
import           Data.MExpr (emify, deemify)
import qualified Data.MExpr.Parser as Parser
import           Data.MExpr.Radix46 (encode46, decode46)
import qualified Data.Map as Map
import           Data.Word (Word64)
import           Metacore.Eval.Eval (Env, Metavalue, eval)
import           System.Environment (getArgs)
import           System.IO (hFlush, stdout)
import           Text.Megaparsec (parse, errorBundlePretty)
import           Text.Read (readMaybe)

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
                        Right (x, env'') -> (fmap (show . emify) x, env'')
    traverse_ putStrLn out
    loop env'

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--encode-46", x] ->
      putStrLn $ case readMaybe x of
        Nothing -> "Invalid."
        Just (x' :: Word64) -> encode46 x'
    ["--decode-46", x] ->
      putStrLn $ case decode46 x of
        Nothing -> "Invalid."
        Just (x' :: Word64) -> show x'
    _ -> loop Map.empty
