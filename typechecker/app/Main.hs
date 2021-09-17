module Main where

--import Checker
import Inexhaustive

get :: Inexhaustive (Maybe String) String
get = inexhaustive (\(Just x) -> x)

main :: IO ()
main = do
  print $ runToMaybe get $ Just "Sylvester"
  putStrLn "--"
  print $ runToMaybe get Nothing
  --let add = Symbol "+"
  --let addT = TFun TInt (TFun TInt TInt)
  --let addition = Appl (Variable (Symbol "+")) (Variable (Symbol "x"))
  --let lambda  = Lambda (Symbol "x") addition
  --let expr = Appl (Annotate lambda addT) (IntLit 5)
  ---- (x + _)(5)
  --putStrLn (show (synthesize (Context [(add, addT)]) expr))
