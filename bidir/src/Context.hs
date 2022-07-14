module Context
  ( Context, noContext
  , lookupVar, containsTypeVar
  , wellFormed
  ) where

import AST

data ContextItem        =
  TypeVarInContext Name |
  TypeKnown Name Type   --

newtype Context = Context [ContextItem]

noContext :: Context
noContext = Context []

data Error                  =
  NotInContext Name Context --

lookupVar :: Name -> Context -> Either Error Type
lookupVar needle (Context (TypeKnown name ty : context)) =
  if (needle == name)
  then Right ty
  else lookupVar needle (Context context)
lookupVar x context = Left $ NotInContext x context

containsTypeVar :: Name -> Context -> Either Error ()
containsTypeVar needle (Context (TypeVarInContext ty : context)) =
  if (needle == ty)
  then Right ()
  else containsTypeVar needle (Context context)
containsTypeVar x context = Left $ NotInContext x context

wellFormed :: Context -> Type -> Either Error ()
wellFormed context (TerminalType (TypeVar x)) =
  containsTypeVar x context
wellFormed _ (TerminalType UnitType) = return ()
wellFormed (Context context) (Forall name ty) =
  wellFormed (Context (TypeVarInContext name : context)) ty
wellFormed context (FunctionType input output) = do
  wellFormed context input
  wellFormed context output
