module Main where

import qualified Data.MExpr.Parser as Parser
import           System.IO (hFlush, stdout)
import           Text.Megaparsec (parseTest)

main :: IO ()
main = do
  putStr "> "
  hFlush stdout
  text <- getLine
  if text == ":q"
  then putStrLn "Goodbye!"
  else do
    parseTest Parser.mexpr text
    main
