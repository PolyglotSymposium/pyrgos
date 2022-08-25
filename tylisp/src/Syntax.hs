module Syntax
  ( Symbol, Keyword(..)
  , DataExpr(..)
  , Expr(..)
  , Monotype(..), subtype
  , TypeExpr(..), forall
  , Disjoint, disjoint
  ) where

import Control.Applicative
import Control.Monad
import Data.Functor

type Symbol = String
newtype Keyword = Keyword String deriving Eq

data DataExpr             =
  Symbol Symbol           |
  KeywordExpr Keyword     |
  Cons DataExpr DataExpr  |
  Nil                     --

data Expr                  =
  -- Corresponds to (disjoint expr f...) in code
  DisjointExpr Expr [Expr] |
  Lambda Symbol Expr       |
  DataExpr                 --

data Monotype              =
  TVar Symbol              |
  TSymbol                  |
  TSpecificKeyword Keyword |
  TKeyword                 |
  TCons Monotype Monotype  |
  (:->:) Monotype Monotype |
  TSExpr                   |
  TDisjoint Disjoint       |
  TNil                     --
  deriving Eq

subtype :: Monotype -> Monotype -> Maybe ()
-- TODO should be able to express "if the thing equals itself, it is an improper
-- subtype" abstractly rather than all these concrete rules
subtype TNil TNil = Just ()
subtype TNil TSExpr = Just ()
subtype TSymbol TSymbol = Just ()
subtype TSymbol TSExpr = Just ()
subtype TKeyword TKeyword = Just ()
subtype TKeyword TSExpr = Just ()
subtype (TSpecificKeyword x) (TSpecificKeyword y) = guard (x == y)
subtype (TSpecificKeyword _) TKeyword = Just ()
-- TODO Data structure sucks for this operation
subtype (TSpecificKeyword _) TSExpr = Just ()
subtype TSExpr TSExpr = Just ()
subtype (TCons a1 b1) (TCons a2 b2) = do
  subtype a1 a2
  subtype b1 b2
subtype (TCons a1 b1) TSExpr = Just ()
subtype (a1 :->: b1) (a2 :->: b2) = do
  subtype a2 a1
  subtype b1 b2
subtype _ _ = Nothing -- TODO deal with TVar

data TypeExpr                =
  Monotype Monotype            |
  TypeScheme { typeVar :: Symbol
             , subtypeOf :: Maybe Monotype
             , bodyType :: TypeExpr
             }                   --

forall :: Symbol -> TypeExpr -> TypeExpr
forall x = TypeScheme x Nothing

newtype Disjoint = Disjoint [Monotype] deriving Eq

disjoint2 :: Monotype -> Monotype -> Maybe ()
disjoint2 (TVar _) _ = Nothing
disjoint2 _ (TVar _) = Nothing
disjoint2 (_ :->: _) _ = Nothing
disjoint2 _ (_:->:  _) = Nothing
disjoint2 TSExpr _ = Nothing
disjoint2 _ TSExpr = Nothing
disjoint2 (TDisjoint _) _ = Nothing
disjoint2 _ (TDisjoint _) = Nothing
disjoint2 TNil TNil = Nothing
disjoint2 TSymbol TSymbol = Nothing
disjoint2 TKeyword TKeyword = Nothing
disjoint2 (TSpecificKeyword x) (TSpecificKeyword y) = guard (x /= y)
disjoint2 (TCons xa xb) (TCons ya yb) =
  guard (xa /= ya) <|> guard (xb /= yb)
disjoint2 _ _ = Just ()

disjoint :: [Monotype] -> Maybe Disjoint
disjoint tys =
  foldM_ disjointComparedTo tys tys
  $> Disjoint tys
  where
  disjointComparedTo :: [Monotype] -> Monotype -> Maybe [Monotype]
  disjointComparedTo others point =
    let result = foldMap (disjoint2 point) others
        others' =
          case others of
            [] -> []
            _ : xs -> xs
    in result $> others'
