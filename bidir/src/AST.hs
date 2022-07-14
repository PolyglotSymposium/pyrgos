{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
module AST
  ( Name
  , TerminalType(..)
  , Poly
  , Ty(..)
  , Monotype
  , Polytype
  , Expr(..)
  ) where

import Data.Kind

type Name = String

data TerminalType =
  UnitType        |
  TypeVar Name    --

data Poly

data Ty :: Type -> Type where
  TerminalType :: TerminalType -> Ty a
  Forall :: Name -> Ty Poly -> Ty Poly
  FunctionType :: Ty a -> Ty a -> Ty a

type Polytype = Ty Poly
type Monotype = Ty ()

data Expr               =
  Var Name              |
  Unit                  |
  Lambda Name Expr      |
  Apply Expr Expr       |
  Ascribe Expr Polytype --
