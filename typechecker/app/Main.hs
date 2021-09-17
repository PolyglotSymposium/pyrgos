module Main where

import Checker

main :: IO ()
main = do
  let add = Symbol "+"
  let addT = TFun TInt (TFun TInt TInt)
  let addition = Appl (Variable (Symbol "+")) (Variable (Symbol "x"))
  let lambda  = Lambda (Symbol "x") addition
  let expr = Appl (Annotate lambda addT) (IntLit 5)
  -- (x + _)(5)
  let t = synthesize (Context [(add, addT)]) expr
  putStrLn $ maybe "Failed to typecheck." (\x -> ": " ++ printType x) t
