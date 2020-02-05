{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.State (StateT, mapStateT, runStateT)
import           Data.Bifunctor (first)
import           Data.Foldable (traverse_)
import           Data.Functor.Identity (Identity(..))
import           Data.MExpr (emify)
import           Data.MExpr.Parser (Parser)
import qualified Metacore.Eval.Parser as EvalParser
import           Metacore.Eval.AST (TopLevel)
import qualified Metacore.Human.Parser as HumanParser
import           Data.MExpr.Radix46 (encode46, decode46)
import qualified Data.Map as Map
import           Data.Word (Word64)
import           Metacore.Eval.Eval (Env, Metavalue, eval)
import           System.Environment (getArgs)
import           System.IO (hFlush, stdout)
import           Text.Megaparsec (parse, errorBundlePretty)
import           Text.Read (readMaybe)

type Rep = StateT Env (Either String)

data Lang = MetaEval | MetaHuman

selectParser :: Lang -> Parser TopLevel
selectParser MetaEval = EvalParser.topLevel
selectParser MetaHuman = HumanParser.topLevel

evalAndPrint :: Lang -> String -> Rep (Maybe Metavalue)
evalAndPrint lang input = do
  let parsed = parse (selectParser lang) "repl" input
  toplevel <- lift $ first errorBundlePretty parsed
  x <- mapStateT (\(Identity x) -> Right x) (eval toplevel)
  lift $ traverse (Left . show . emify) x

loop :: Lang -> Env -> IO ()
loop lang env = do
  putStr "> "
  hFlush stdout
  text <- getLine
  if text == ":q"
  then putStrLn "Goodbye!"
  else do
    let (out, env') = case runStateT (evalAndPrint lang text) env of
                        Left e -> (Just e, env)
                        Right (x, env'') -> (fmap (show . emify) x, env'')
    traverse_ putStrLn out
    loop lang env'

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
    ["--repl-eval"] -> loop MetaEval Map.empty
    _ -> loop MetaHuman Map.empty
