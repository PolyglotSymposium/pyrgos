{-# LANGUAGE FlexibleContexts #-}
module Context
  ( Context, emptyContext
  , extendWithBoundTypeVar
  , extendWithAscription
  , extendWithUnsolved
  , extendWithSolved
  , extendWithScopeMarker
  , wellFormedPolytype
  ) where

import AST

import Control.Monad.Error.Class
import Control.Monad.State.Class

data ContextEntry                 =
  -- A universally-bound type variable that is in scope
  -- α
  BoundTypeVar Name               |
  -- A value-level variable in scope whose type is known
  -- x : A
  VarTyping Name Polytype         |
  -- An unsolved existential type variable that is in scope
  -- α^
  UnsolvedExistential Name        |
  -- An solved existential type variable that is in scope
  -- α^ = τ
  SolvedExistential Name Monotype |
  -- A scope marker for an existential type variable
  ScopeMarker Name                --
  deriving Eq

newtype Context = Context [ContextEntry]

emptyContext :: Context
emptyContext = Context []

-- TODO does this exist?
localize :: MonadState s m => m a -> m a
localize s = do
  x' <- get -- store off previous state
  x <- s -- run the localized monadic action
  put x' -- restore the previous state
  return x -- return the result of the monadic action

boundTypeVarInEntry :: Name -> ContextEntry -> Bool
boundTypeVarInEntry x (BoundTypeVar y) = x == y
boundTypeVarInEntry x (VarTyping _ ptype) = _ -- TODO look in ptype for x?
boundTypeVarInEntry _ (UnsolvedExistential _) = False
boundTypeVarInEntry x (SolvedExistential _ mtype) = _ -- TODO look in mtype for x?
boundTypeVarInEntry _ (ScopeMarker _) = False

boundTypeVarInDomain :: Name -> Context -> Bool
boundTypeVarInDomain x (Context entries) =
  elemBy boundTypeVarInEntry entries

extendWithBoundTypeVar :: MonadState Context m => Name -> m ()
extendWithBoundTypeVar name =
  modify (\(Context xs) -> Context $ (BoundTypeVar name) : xs)

extendWithAscription :: MonadState Context m => Name -> Polytype -> m ()
extendWithAscription name ty =
  modify (\(Context xs) -> Context $ (VarTyping name ty) : xs)

extendWithUnsolved :: MonadState Context m => Name -> m ()
extendWithUnsolved name =
  modify (\(Context xs) -> Context $ (UnsolvedExistential name) : xs)

extendWithSolved :: MonadState Context m => Name -> Monotype -> m ()
extendWithSolved name monotype =
  modify (\(Context xs) -> Context $ (SolvedExistential name monotype) : xs)

extendWithScopeMarker :: MonadState Context m => Name -> m ()
extendWithScopeMarker name =
  modify (\(Context xs) -> Context $ (ScopeMarker name) : xs)

elemBound :: (MonadState Context m, MonadError String m)
            => Name -> m ()
elemBound x = do
  Context context <- get
  if elem (BoundTypeVar x) context
  then return ()
  else throwError ("Bound type variable not in context" {-x context-})

isExistential :: ContextEntry -> Maybe Name
isExistential (UnsolvedExistential x) = Just x
isExistential (SolvedExistential x _) = Just x
isExistential _ = Nothing

elemExistential :: (MonadState Context m, MonadError String m)
                => Name -> m ()
elemExistential x = do
  Context context <- get
  if any (\e -> isExistential e == Just x) context
  then return ()
  else throwError ("Existential type variable not in context" {-x context-})

wellFormedPolytype :: (MonadState Context m, MonadError String m)
                   => Polytype -> m ()
wellFormedPolytype (PolyTerminalType UnitType) = return ()
wellFormedPolytype (PolyTerminalType (UniversalTypeVar x)) = elemBound x
wellFormedPolytype (PolyTerminalType (ExistentialTypeVar x)) = elemExistential x
wellFormedPolytype (PolyFunctionType a b) = do
  wellFormedPolytype a
  wellFormedPolytype b
wellFormedPolytype (Forall binding body) = do
  localize $ extendWithBoundTypeVar binding
  wellFormedPolytype body
