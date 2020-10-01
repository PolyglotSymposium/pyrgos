module Main where

import Ennalleen.Parser
import System.IO (hFlush, stdout)
import Text.Megaparsec (parse, errorBundlePretty)

parseAndPrint :: String -> String
parseAndPrint = either errorBundlePretty show . parse expr "<input>"

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
