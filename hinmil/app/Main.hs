module Main (main) where

import AlgorithmW
import Assumptions
import Repl
import Substitutions
import TypeAST
import TypeSchemes
import Unification

import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty(..))
import System.Environment (getArgs)

printUResult :: String -> Either UnificationFailure Substitutions -> String
printUResult name (Left failure) = name ++ " ERROR: " ++ printUFailure failure
printUResult name (Right substs) = name ++ " MGU: " ++ printSubsts substs

compareSubstituted :: Term -> Term -> Substitutions -> String
compareSubstituted term1 term2 ss
  | subs ss term1 == subs ss term2 = "Unified!"
  | otherwise = intercalate " "
      [ "left: "
      , printTerm $ subs ss term1
      , "right:"
      , printTerm $ subs ss term2
      , "MGU:"
      , printSubsts ss
      ]

ex1Term1 :: Term
ex1Term1 = TyVar "a"

ex1Term2 :: Term
ex1Term2 = TyApp "f" (TyVar "x" :| [])

ex1 :: Either UnificationFailure Substitutions
ex1 = unify ex1Term1 ex1Term2

ex2Term1 :: Term
ex2Term1 = TyVar "a"

ex2Term2 :: Term
ex2Term2 = TyApp "f" (TyVar "a" :| [])

ex2 :: Either UnificationFailure Substitutions
ex2 = unify ex2Term1 ex2Term2

ex3Term1 :: Term
ex3Term1 = TyApp "f" (TyVar "a" :| [TyVar "b"])

ex3Term2 :: Term
ex3Term2 = TyApp "f" (TyVar "x" :| [])

ex3 :: Either UnificationFailure Substitutions
ex3 = unify ex3Term1 ex3Term2

ex4Term1 :: Term
ex4Term1 = TyApp "f" (TyVar "a" :| [])

ex4Term2 :: Term
ex4Term2 = TyApp "g" (TyVar "x" :| [])

ex4 :: Either UnificationFailure Substitutions
ex4 = unify ex4Term1 ex4Term2

ex5Term1 :: Term
ex5Term1 =
  let
    t1 = TyApp "Vector" (TyVar "A" :| [])
    t2 = TyVar "A"
  -- Either[Vector[A], A]
  in TyApp "Either" (t1 :| [t2])

ex5Term2 :: Term
ex5Term2 =
  let
    t1 = TyVar "B"
    t2 = TyApp "Maybe" (TyVar "C" :| [])
  -- Either[B,Maybe[C]]
  in TyApp "Either" (t1 :| [t2])

-- {Maybe[C]/A, Vector[Maybe[C]]/B}

ex5 :: Either UnificationFailure Substitutions
ex5 = unify ex5Term1 ex5Term2

ex6Term1 :: Term
ex6Term1 =
  let
    t1 = TyApp "Maybe" (TyVar "A" :| [])
    t2 = TyVar "A"
    t3 = TyApp "Either" (TyVar "A" :| [TyVar "E"])
  -- Triple[Maybe[A],A,Either[A, E]]
  in TyApp "Triple" (t1 :| [t2, t3])

ex6Term2 :: Term
ex6Term2 =
  let
    t1 = TyVar "B"
    t2 = TyApp "Maybe"  (TyVar "C" :| [])
    t3 = TyApp "Either" (TyVar "E" :| [TyApp "Maybe" (TyVar "D" :| [])])
  -- Triple[B,Maybe[C],Either[E,Maybe[D]]]
  in TyApp "Triple" (t1 :| [t2, t3])

ex6 :: Either UnificationFailure Substitutions
ex6 = unify ex6Term1 ex6Term2

singletonGamma :: Gamma
singletonGamma = extend "x" (Type $ TyLit "Int") emptyGamma

dualGamma :: Gamma
dualGamma = extend "f" (Forall "a" $ Type (TyVar "a" --> TyVar "a")) singletonGamma

printIResult :: String -> Either InferenceFailure TypeScheme -> String
printIResult name (Left failure) = name ++ " ERROR: " ++ printIFailure failure
printIResult name (Right scheme) = name ++ " MGU: " ++ printTypeScheme scheme

runTests :: IO ()
runTests = do
  putStrLn "UNIFICATION EXAMPLES:"
  putStrLn $ printUResult "1:" ex1
  putStrLn $ printUResult "2:" ex2
  putStrLn $ printUResult "3:" ex3
  putStrLn $ printUResult "4:" ex4
  let ex5part1 = printUResult "5:" ex5
  let ex5part2 = either printUFailure (compareSubstituted ex5Term1 ex5Term2) ex5
  putStrLn $ ex5part1 ++ " -> " ++ ex5part2
  putStrLn "Fails if substitutions combine out of order"
  let ex6part1 = printUResult "6:" ex6
  let ex6part2 = either printUFailure (compareSubstituted ex6Term1 ex6Term2) ex6
  putStrLn $ ex6part1 ++ " -> " ++ ex6part2
  putStrLn "INFERENCE EXAMPLES:"
  putStrLn $ printIResult "7:" $ principal singletonGamma $ Var "x"
  putStrLn $ printIResult "8:" $ principal emptyGamma $ Lambda "x" $ Var "x"
  putStrLn $ printIResult "9:" $ principal dualGamma $ Apply (Var "f") $ Var "x"
  putStrLn $ printIResult "10:" $ principal singletonGamma $ Let "y" (Var "x") (Var "y")
  putStrLn $ printIResult "11:" $ principal singletonGamma $ Lambda "y" $ Var "x"

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--test"] -> runTests
    ["--parser-repl"] -> parserRepl
    ["--repl"] -> repl
    _ -> putStrLn "TODO"
