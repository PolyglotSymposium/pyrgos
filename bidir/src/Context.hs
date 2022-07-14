{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module Context
  ( Context, noContext
  , lookupVar, containsTypeVar
  , wellFormed, isSubtypeOf
  ) where

import AST

import Control.Monad.Error.Class
import Control.Monad.Reader.Class
import Data.Either.Utils

data Context =
  Context { boundTypeVars :: [Name]
          , assumptions :: [(Name, Polytype)]
          }

noContext :: Context
noContext = Context [] []

data Err                               =
  CannotProveSubtypingRelation Name Name |
  NotInContext Name Context              --

lookupVar :: (MonadReader Context m, MonadError Err m) => Name -> m Polytype
lookupVar x = do
  context <- ask
  let result = lookup x $ assumptions context
  maybeToEither (NotInContext x context) result

bindTypeVar :: Name -> Context -> Context
bindTypeVar name context =
  context { boundTypeVars = name : boundTypeVars context }

containsTypeVar :: (MonadReader Context m, MonadError Err m) => Name -> m ()
containsTypeVar x = do
  context <- ask
  if elem x $ boundTypeVars context
  then return ()
  else throwError $ NotInContext x context

wellFormed :: (MonadReader Context m, MonadError Err m) => Polytype -> m ()
-- Variables are well-formed if in context
wellFormed (TerminalType (TypeVar x)) = containsTypeVar x
-- Unit type is always well-formed
wellFormed (TerminalType UnitType) = return ()
-- Universal quantification is well-formed when the quantified type is
-- well-formed in the extended context
wellFormed (Forall name ty) =
  local (bindTypeVar name) $ wellFormed ty
-- A function type is well-formed when both its input and its output are
-- well-formed.
wellFormed (FunctionType input output) = do
  wellFormed input
  wellFormed output

isSubtypeOf :: (MonadReader Context m, MonadError Err m) => Polytype
                                                         -> Polytype -> m ()
-- Reflexivity of subtyping on literal types
isSubtypeOf (TerminalType UnitType) (TerminalType UnitType) = return ()
-- Reflexivity of subtyping on type variables
isSubtypeOf (TerminalType (TypeVar name1)) (TerminalType (TypeVar name2)) =
  if (name1 == name2)
  then containsTypeVar name1
  else throwError $ CannotProveSubtypingRelation name1 name2
-- The subtyping relation on functions requires contravariance in inputs and
-- covariance in outputs
isSubtypeOf (FunctionType input1 output1) (FunctionType input2 output2) = do
  -- Contravariance of inputs
  isSubtypeOf input2 input1
  -- Covariance of outputs
  isSubtypeOf output1 output2
-- TODO left and right universal quantification rules
isSubtypeOf _ _ = return ()
