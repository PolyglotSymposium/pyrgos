module TypeAST
  ( Name, Term(..), printTerm
  ) where

import Control.Monad (join)
import Data.List (intersperse)
import Data.List.NonEmpty (NonEmpty(..), toList)

type Name = String

data Term = TyVar Name | TyApp Name (NonEmpty Term) deriving Eq

printTerm :: Term -> String
printTerm (TyVar name) = name
printTerm (TyApp name args) = name ++ "(" ++ join (intersperse "," $ printTerm <$> toList args) ++ ")"
