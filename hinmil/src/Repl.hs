module Repl (repl, parserRepl) where

import AlgorithmW
import Assumptions
import TypeSchemes
import qualified Parser

import Data.Either.Combinators (mapLeft)
import Control.Monad (unless)
import Data.Function ((&))
import System.IO
import Text.Megaparsec (parse, errorBundlePretty)

parseAndPrint :: String -> IO ()
parseAndPrint input =
  parse Parser.expr "cmd" input
  & either errorBundlePretty show
  & putStrLn

parseInferPrint :: String -> IO ()
parseInferPrint input = putStrLn $ either id id $ do
  ast <- parse Parser.expr "cmd" input & mapLeft errorBundlePretty
  scheme <- principal emptyGamma ast & mapLeft printIFailure
  return $ show ast ++ " : " ++ printTypeScheme scheme

promptAndRead :: IO String
promptAndRead = putStr "hinmil> "
  >> hFlush stdout
  >> getLine

runReplWith :: (String -> IO ()) -> IO ()
runReplWith f = do
  input <- promptAndRead
  unless (input == ":q")
    $ f input
    >> runReplWith f

repl :: IO ()
repl = runReplWith parseInferPrint

parserRepl :: IO ()
parserRepl = runReplWith parseAndPrint
