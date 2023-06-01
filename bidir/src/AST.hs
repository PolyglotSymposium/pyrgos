{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
module AST
  ( Name
  , TerminalType(..)
  -- , Poly
  -- , Ty(..)
  , Monotype(..), monotypeIsExistential
  , Polytype(..), substituteForUniversal
  , liftToPoly, polyIsMono
  , Expr(..)
  ) where

-- import Data.Kind

type Name = String

data TerminalType         =
  UnitType                |
  UniversalTypeVar Name   |
  ExistentialTypeVar Name --
  deriving Eq

--data Poly
--
--data Ty :: Type -> Type where
--  TerminalType :: TerminalType -> Ty a
--  Forall :: Name -> Ty Poly -> Ty Poly
--  FunctionType :: Ty a -> Ty a -> Ty a
--
--type Polytype = Ty Poly
--type Monotype = Ty ()

data Polytype                        =
  PolyTerminalType TerminalType      |
  Forall Name Polytype               |
  PolyFunctionType Polytype Polytype --
  deriving Eq

data Monotype                        =
  MonoTerminalType TerminalType      |
  MonoFunctionType Monotype Monotype --
  deriving Eq

monotypeIsExistential :: Monotype -> Maybe Name
monotypeIsExistential (MonoTerminalType (ExistentialTypeVar name)) = Just name
monotypeIsExistential _ = Nothing

polyIsMono :: Polytype -> Maybe Monotype
polyIsMono (PolyTerminalType tt) = Just $ MonoTerminalType tt
polyIsMono (Forall _ _) = Nothing
polyIsMono (PolyFunctionType a b) = do
  a' <- polyIsMono a
  b' <- polyIsMono b
  return $ MonoFunctionType a' b'

liftToPoly :: Monotype -> Polytype
liftToPoly (MonoTerminalType tt) =
  PolyTerminalType tt
liftToPoly (MonoFunctionType a b) =
  PolyFunctionType (liftToPoly a) (liftToPoly b)

-- When you hit a universal with `Name`, substitute in this `TerminalType` in
-- `Polytype`, producing `Polytype`.
substituteForUniversal :: Name -> TerminalType -> Polytype -> Polytype
substituteForUniversal = undefined -- TODO

data Expr               =
  Var Name              |
  Unit                  |
  Lambda Name Expr      |
  Apply Expr Expr       |
  Ascribe Expr Polytype --
