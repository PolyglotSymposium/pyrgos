module Main (main) where

import Metacore.AST
import qualified Data.MExpr.Parser as Parser
import           System.IO (hFlush, stdout)
import Text.Megaparsec (parse, errorBundlePretty)

parseMetacore :: String -> String
parseMetacore input =
  case parse Parser.mexpr "repl" input of
    Left e -> errorBundlePretty e
    Right x -> show $ ofMExpr x

main :: IO ()
main = do
  putStr "> "
  hFlush stdout
  text <- getLine
  if text == ":q"
  then putStrLn "Goodbye!"
  else do
    putStrLn $ parseMetacore text
    main
