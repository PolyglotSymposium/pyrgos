module Main where

import Lib
import Data.List.NonEmpty (NonEmpty(..))

s1 :: Substitutions
s1 = sub1 $ Subst (TyVar "a") "x"

s2 :: Substitutions
s2 = sub1 $ Subst (TyApp "f" (TyVar "b" :| [TyVar "x"])) "y"

s3 :: Substitutions
s3 = s2 <> s1

main :: IO ()
main = putStrLn $ printSubsts s3
