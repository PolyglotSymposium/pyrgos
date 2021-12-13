module Main where

import Lib
import Data.List.NonEmpty (NonEmpty(..))

printUResult :: Either UnificationFailure Substitutions -> String
printUResult (Left failure) = "ERROR: " ++ printUFailure failure
printUResult (Right substs) = "MGU: " ++ printSubsts substs

s1 :: Substitutions
s1 = sub1 $ Subst (TyVar "a") "x"

s2 :: Substitutions
s2 = sub1 $ Subst (TyApp "f" (TyVar "b" :| [TyVar "x"])) "y"

s3 :: Substitutions
s3 = sub1 $ Subst (TyApp "g" (TyVar "y" :| [TyVar "x", TyVar "a"])) "z"

sLeft :: Substitutions
sLeft = s3 <> s2

sRight :: Substitutions
sRight = s2 <> s1

sTotalL :: Substitutions
sTotalL = sLeft <> s1

sTotalR :: Substitutions
sTotalR = s3 <> sRight

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
  in TyApp "Either" (t1 :| [t2])

ex5Term2 :: Term
ex5Term2 =
  let
    t1 = TyVar "B"
    t2 = TyApp "Maybe" (TyVar "C" :| [])
  in TyApp "Either" (t1 :| [t2])

-- {Maybe[C]/A, Vector[Maybe[C]]/B}

ex5 :: Either UnificationFailure Substitutions
ex5 = unify ex5Term1 ex5Term2

main :: IO ()
main = do
  putStrLn $ printUResult ex1
  putStrLn $ printUResult ex2
  putStrLn $ printUResult ex3
  putStrLn $ printUResult ex4
  putStrLn $ printUResult ex5
