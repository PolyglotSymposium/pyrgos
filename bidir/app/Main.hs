module Main (main) where

import Context
import AST

main :: IO ()
main =
  case wellFormed noContext (TerminalType UnitType) of
    Right () -> putStrLn "Unit type always well-formed"
    Left _ -> putStrLn "BUG: unit type not well-formed!"
