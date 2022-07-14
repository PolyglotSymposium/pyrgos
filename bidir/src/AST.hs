module AST (Name, TerminalType(..), Type(..), Monotype(..), Expr(..)) where

type Name = String

data TerminalType =
  UnitType        |
  TypeVar Name    --

data Type                   =
  TerminalType TerminalType |
  Forall Name Type          |
  FunctionType Type Type    --

data Monotype                    =
  Monoterminal TerminalType      |
  Monofunction Monotype Monotype --

data Expr           =
  Var Name          |
  Unit              |
  Lambda Name Expr  |
  Apply Expr Expr   |
  Ascribe Expr Type --
