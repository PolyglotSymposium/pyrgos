module Syntax where

type Symbol = String
type Keyword = String

data Expr         =
  Symbol Symbol   |
  Keyword Keyword |
  Cons Expr Expr  |
  Nil             --

data RawType             =
  TSymbol                |
  TKeyword Keyword       |
  TCons RawType RawType  |
  TNil                   --
  deriving Eq
