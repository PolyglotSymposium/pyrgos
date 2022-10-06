module Main (main) where

import Context
import AST
import Control.Monad.State

main :: IO ()
main = do
  case runStateT (wellFormedPolytype $ PolyTerminalType UnitType) emptyContext of
    Right ((), _) -> putStrLn "Unit type always well-formed"
    Left x -> putStrLn x
