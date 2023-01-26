{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
module AST
  ( Name, EName
  , TerminalType(..)
  -- , Poly
  -- , Ty(..)
  , Monotype(..), monotypeIsExistential
  , Polytype(..)
  , liftToPoly, polyIsMono
  , Expr(..)
  , MonadFreshEVar (freshEVar)
  ) where

-- import Data.Kind
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Control.Monad.IO.Class (MonadIO, liftIO)

type Name = String

data EName                =
  SourceCodeName Name     |
  InternalName   UUID     --
  deriving Eq

class Monad m => MonadFreshEVar m where
  freshEVar :: m EName

instance (Monad m, MonadIO m) => MonadFreshEVar m where
  freshEVar = liftIO $ InternalName <$> nextRandom

data TerminalType          =
  UnitType                 |
  UniversalTypeVar Name    |
  ExistentialTypeVar EName --
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

monotypeIsExistential :: Monotype -> Maybe EName
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

data Expr               =
  Var Name              |
  Unit                  |
  Lambda Name Expr      |
  Apply Expr Expr       |
  Ascribe Expr Polytype --
