{-# LANGUAGE FlexibleContexts #-}
module Context
  ( Context, emptyContext
  , extendWithBoundTypeVar
  , extendWithVarTyping
  , extendWithUnsolved
  , extendWithSolved
  , extendWithScopeMarker
  , wellFormedPolytype
  , substituteSolved
  ) where

import AST

import Control.Monad.Error.Class
import Control.Monad.State.Class
import Control.Monad (guard)
import Data.Functor (($>))
import Data.Function ((&))
import Data.Maybe (mapMaybe, listToMaybe)

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
boundTypeVarInEntry _x (VarTyping _ _ptype) = undefined -- TODO look in ptype for x?
boundTypeVarInEntry _ (UnsolvedExistential _) = False
boundTypeVarInEntry _x (SolvedExistential _ _mtype) = undefined -- TODO look in mtype for x?
boundTypeVarInEntry _ (ScopeMarker _) = False

-- α ∈ dom(Γ) for UvarCtx rule
boundTypeVarInDomain :: Name -> Context -> Bool
boundTypeVarInDomain x (Context entries) =
  any (boundTypeVarInEntry x) entries

valueVarInEntry :: Name -> ContextEntry -> Bool
valueVarInEntry x (VarTyping y _) = x == y
valueVarInEntry _ _ = False

-- x ∈ dom(Γ) for VarCtx rule
valueVarInDomain :: Name -> Context -> Bool
valueVarInDomain x (Context entries) =
  any (valueVarInEntry x) entries

existentialInEntry :: Name -> ContextEntry -> Bool
existentialInEntry _ (BoundTypeVar _) = False
existentialInEntry _x (VarTyping _ _ptype) = undefined -- TODO look in ptype for x?
existentialInEntry x (UnsolvedExistential y) = x == y
existentialInEntry x (SolvedExistential y _mtype) = x == y -- TODO look in mtype for x?
existentialInEntry x (ScopeMarker y) = x == y

-- α^ ∈ dom(Γ) for EvarCtx, SolvedEvarCtx rules
existentialInDomain :: Name -> Context -> Bool
existentialInDomain x (Context entries) =
  any (existentialInEntry x) entries

extendWithBoundTypeVar :: MonadState Context m => Name -> m ()
extendWithBoundTypeVar name =
  let _ = boundTypeVarInDomain -- TODO
  in modify (\(Context xs) -> Context $ (BoundTypeVar name) : xs)

extendWithVarTyping :: MonadState Context m => Name -> Polytype -> m ()
extendWithVarTyping name ty =
  let _ = valueVarInDomain -- TODO
  in modify (\(Context xs) -> Context $ (VarTyping name ty) : xs)

extendWithUnsolved :: MonadState Context m => Name -> m ()
extendWithUnsolved name =
  let _ = existentialInDomain -- TODO
  in modify (\(Context xs) -> Context $ (UnsolvedExistential name) : xs)

extendWithSolved :: MonadState Context m => Name -> Monotype -> m ()
extendWithSolved name monotype =
  let _ = existentialInDomain -- TODO
  in modify (\(Context xs) -> Context $ (SolvedExistential name monotype) : xs)

extendWithScopeMarker :: MonadState Context m => Name -> m ()
extendWithScopeMarker name =
  let _ = existentialInDomain -- TODO
  in modify (\(Context xs) -> Context $ (ScopeMarker name) : xs)

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

solution :: Context -> Name -> Maybe Monotype
solution (Context ctxt) existential = mapMaybe solution' ctxt & listToMaybe where
  solution' :: ContextEntry -> Maybe Monotype
  solution' (SolvedExistential x mtype) = guard (x == existential) $> mtype
  solution' _ = Nothing

-- Figure 8
substituteSolved :: Context -> Polytype -> Polytype
substituteSolved _ x@(PolyTerminalType UnitType) = x
substituteSolved _ x@(PolyTerminalType (UniversalTypeVar _)) = x
substituteSolved ctxt t@(PolyTerminalType (ExistentialTypeVar x)) =
  solution ctxt x
  & maybe t (substituteSolved ctxt . liftToPoly)
substituteSolved ctxt (Forall x ptype) = Forall x $ substituteSolved ctxt ptype
substituteSolved ctxt (PolyFunctionType a b) =
  PolyFunctionType (substituteSolved ctxt a) (substituteSolved ctxt b)
