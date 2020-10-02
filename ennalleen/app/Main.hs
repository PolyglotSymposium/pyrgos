{-# LANGUAGE TypeApplications #-}
module Main where

import Ennalleen.Syntax
import Ennalleen.Parser
import System.IO (hFlush, stdout)

parseAndPrint :: String -> String
parseAndPrint = either id (show @Expr). parseExpr

loop :: IO ()
loop = do
  putStr "> "
  hFlush stdout
  text <- getLine
  if text == ":q"
  then putStrLn "Goodbye!"
  else do
    putStrLn $ parseAndPrint text
    loop

main :: IO ()
main = loop
