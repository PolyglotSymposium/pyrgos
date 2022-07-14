module Main (main) where

import Context
import AST
import Control.Monad.Reader

main :: IO ()
main =
  case runReaderT (wellFormed $ TerminalType UnitType) noContext of
    Right () -> putStrLn "Unit type always well-formed"
    Left _ -> putStrLn "BUG: unit type not well-formed!"
