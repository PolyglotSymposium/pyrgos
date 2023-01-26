{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
module Context
  ( Context, emptyContext
  , extendWithUniversal
  , extendWithVarTyping
  , extendWithUnsolved
  , extendWithSolved
  , wellFormedPolytype
  , substituteSolved, substituteForUniversal
  , truncateContextE, truncateContextA
  , refineExistialAsFunction
  , splitContextE
  ) where

import AST

import Control.Monad.Error.Class
import Control.Monad.State.Class
import Control.Monad (guard)
import Data.Functor (($>))
import Data.Function ((&))
import Data.Maybe (mapMaybe, listToMaybe)
import Data.List (elemIndex)

data ContextEntry                   =
  -- A universally-bound type variable that is in scope
  -- α
  BoundTypeVar Name                 |
  -- A value-level variable in scope whose type is known
  -- x : A
  VarTyping Name Polytype           |
  -- Either of (depending on the `Maybe`):
  -- 1. An unsolved existential type variable that is in scope
  --    α^
  -- 2. An solved existential type variable that is in scope
  --    α^ = τ
  Existential EName (Maybe Monotype) --
  -- We do not believe it is necessary to execution to model scope markers as
  -- separate context entries. This appears to be an artifact of the syntax of
  -- the paper---to make it readable.
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
boundTypeVarInEntry _ (Existential _ Nothing) = False
boundTypeVarInEntry _x (Existential _ (Just _mtype)) = undefined -- TODO look in mtype for x?

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

existentialInEntry :: EName -> ContextEntry -> Bool
existentialInEntry _ (BoundTypeVar _) = False
existentialInEntry _x (VarTyping _ _ptype) = undefined -- TODO look in ptype for x?
existentialInEntry x (Existential y _mtype) = x == y -- TODO look in mtype for x?

-- α^ ∈ dom(Γ) for EvarCtx, SolvedEvarCtx rules
existentialInDomain :: EName -> Context -> Bool
existentialInDomain x (Context entries) =
  any (existentialInEntry x) entries

extendWithUniversal :: MonadState Context m => Name -> m ()
extendWithUniversal name =
  let _ = boundTypeVarInDomain -- TODO
  in modify (\(Context xs) -> Context $ (BoundTypeVar name) : xs)

extendWithVarTyping :: MonadState Context m => Name -> Polytype -> m ()
extendWithVarTyping name ty =
  let _ = valueVarInDomain -- TODO
  in modify (\(Context xs) -> Context $ (VarTyping name ty) : xs)

extendWithUnsolved :: MonadState Context m => EName -> m ()
extendWithUnsolved name =
  let _ = existentialInDomain -- TODO
  in modify (\(Context xs) -> Context $ (Existential name Nothing) : xs)

-- TODO Should this really be exposed? We don't ever extend with a solved,
-- really, in practice; we solve an unsolved...
extendWithSolved :: MonadState Context m => EName -> Monotype -> m ()
extendWithSolved name monotype =
  let _ = existentialInDomain -- TODO
  in modify (\(Context xs) -> Context $ (Existential name (Just monotype)) : xs)

elemBound :: (MonadState Context m, MonadError String m)
            => Name -> m ()
elemBound x = do
  Context context <- get
  if elem (BoundTypeVar x) context
  then return ()
  else throwError ("Bound type variable not in context" {-x context-})

isExistential :: ContextEntry -> Maybe EName
isExistential (Existential x _) = Just x
isExistential _ = Nothing

elemExistential :: (MonadState Context m, MonadError String m)
                => EName -> m ()
elemExistential x = do
  Context context <- get
  if any (\e -> isExistential e == Just x) context
  then return ()
  else throwError ("Existential type variable not in context" {-x context-})

maybeSplitAt :: Eq a => a -> [a] -> Maybe ([a], [a])
maybeSplitAt pred xs = do
  index <- elemIndex pred xs
  return $ splitAt (index - 1) xs

refineExistialAsFunction :: (MonadState Context m, MonadError String m, MonadFreshEVar m)
                         => EName -> m (EName, EName)
refineExistialAsFunction x = do
  Context context <- get
  let errorMsg = "Existential type variable not in context" {-x context-}
  (newer, older) <-
    maybeSplitAt (Existential x Nothing) context
    & maybe (throwError errorMsg) return
  let older' = drop 1 older
  a1 <- freshEVar
  a2 <- freshEVar
  let splice = [ Existential x (Just $ MonoFunctionType (MonoTerminalType (ExistentialTypeVar a1)) (MonoTerminalType (ExistentialTypeVar a2)))
               , Existential a1 Nothing
               , Existential a2 Nothing
               ]
  let context' = newer ++ splice ++ older'
  put $ Context context'
  return (a1, a2)

wellFormedPolytype :: (MonadState Context m, MonadError String m)
                   => Polytype -> m ()
wellFormedPolytype (PolyTerminalType UnitType) = return ()
wellFormedPolytype (PolyTerminalType (UniversalTypeVar x)) = elemBound x
wellFormedPolytype (PolyTerminalType (ExistentialTypeVar x)) = elemExistential x
wellFormedPolytype (PolyFunctionType a b) = do
  wellFormedPolytype a
  wellFormedPolytype b
wellFormedPolytype (Forall binding body) = do
  localize $ extendWithUniversal binding
  wellFormedPolytype body

solution :: Context -> EName -> Maybe Monotype
solution (Context ctxt) existential = mapMaybe solution' ctxt & listToMaybe where
  solution' :: ContextEntry -> Maybe Monotype
  solution' (Existential x (Just mtype)) = guard (x == existential) $> mtype
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

-- When you hit a universal with `Name`, substitute in this `TerminalType` in
-- `Polytype`, producing `Polytype`.
substituteForUniversal :: Name -> TerminalType -> Polytype -> Polytype
substituteForUniversal = undefined -- TODO

splitContextE :: EName -> Context -> Maybe (Context, Context)
splitContextE existential (Context context) = do
  (newer, older) <- maybeSplitAt (Existential existential Nothing) context
  return (Context newer, Context older)

-- TODO rewrite with splitContextE?
truncateContextE :: (MonadState Context m, MonadError String m) => EName -> m ()
truncateContextE existential = do
  Context context <- get
  context' <- truncateContextE' context
  put (Context context')
  where
  truncateContextE' :: MonadError String m => [ContextEntry] -> m [ContextEntry]
  truncateContextE' (Existential x _ : ctxt) =
    if existential == x
    then return ctxt
    else truncateContextE' ctxt
  truncateContextE' (_ : ctxt) = truncateContextE' ctxt
  truncateContextE' [] = throwError "BUG: failed to truncate context existentially"

-- TODO write a splitContextA and rewrite in terms of that?
truncateContextA :: (MonadState Context m, MonadError String m) => Name -> m ()
truncateContextA universal = do
  Context context <- get
  context' <- truncateContextA' context
  put (Context context')
  where
  truncateContextA' :: MonadError String m => [ContextEntry] -> m [ContextEntry]
  truncateContextA' (BoundTypeVar x : ctxt) =
    if universal == x
    then return ctxt
    else truncateContextA' ctxt
  truncateContextA' (_ : ctxt) = truncateContextA' ctxt
  truncateContextA' [] = throwError "BUG: failed to truncate context universally"
