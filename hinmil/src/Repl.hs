module Repl (typeRepl, parserRepl) where

import AlgorithmW
import Assumptions
import Expr
import TypeSchemes
import qualified Parser

import Control.Monad (unless)
import Control.Monad.Error.Class (liftEither)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except
import Data.Bifunctor (first)
import Data.Function ((&))
import Data.IORef
import System.IO
import Text.Megaparsec (parse, errorBundlePretty)

parseAndPrint :: String -> IO ()
parseAndPrint input =
  parse Parser.topLevel "cmd" input
  & either errorBundlePretty show
  & putStrLn

inferAndShow :: IORef Gamma -> TopLevel -> ExceptT String IO String
inferAndShow assumptionsRef (TLExpr expr) = do
  assumptions <- liftIO $ readIORef assumptionsRef
  scheme <- principal assumptions expr & withExceptT printIFailure
  return $ show expr ++ " : " ++ printTypeScheme scheme
inferAndShow assumptionsRef (TLDecl (Define name expr)) = do
  assumptions <- liftIO $ readIORef assumptionsRef
  scheme <- principal assumptions expr & withExceptT printIFailure
  liftIO $ writeIORef assumptionsRef $ extend name scheme assumptions
  -- TODO print assumptions?
  return $ name ++ " : " ++ printTypeScheme scheme

parseInferPrint :: IORef Gamma -> String -> IO ()
parseInferPrint assumptionsRef input = do
  e <- runExceptT $ do
    ast <- liftEither $ parse Parser.topLevel "cmd" input & first errorBundlePretty
    inferAndShow assumptionsRef ast
  putStrLn $ either id id e

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

typeRepl :: IO ()
typeRepl = do
  ref <- newIORef emptyGamma
  runReplWith $ parseInferPrint ref

parserRepl :: IO ()
parserRepl = runReplWith parseAndPrint
