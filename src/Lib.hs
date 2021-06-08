module Lib
    ( Context(..), Type(..), Expr(..), synthesize, Symbol(..)
    ) where

import Control.Monad (guard)
import Control.Applicative ((<|>))

data Type
  = TInt
  | TStr
  | TFun Type Type
  deriving (Eq, Show)

newtype Symbol = Symbol String deriving Eq

data Expr
  = IntLit Integer -- 5
  | StrLit String -- "foo"
  | Lambda Symbol Expr -- x => y
  | Appl Expr Expr -- f(x)
  | Annotate Expr Type -- x: Boolean
  | Variable Symbol -- x

{- For a literal that is syntactically an integer, synthesize type Integer for
   it w.r.t. any typing context -}
synthInt :: Context -> Expr -> Maybe Type
synthInt _ (IntLit _) = Just TInt
synthInt _ _ = Nothing

{- For a literal that is syntactically a string, synthesize type String for it
   w.r.t. any typing context -}
synthStr :: Context -> Expr -> Maybe Type
synthStr _ (StrLit _) = Just TStr
synthStr _ _ = Nothing

newtype Context = Context [(Symbol, Type)]

lookupName :: Symbol -> Context -> Maybe Type
lookupName symbol (Context context) = lookup symbol context

augment :: Symbol -> Type -> Context -> Context
augment s t (Context context) = Context ((s, t) : context)

{- Finding a variable in the typing context suffices to synthesize its type. -}
-- x
-- x => x
-- x :: Int
synthVar :: Context -> Expr -> Maybe Type
synthVar context (Variable x) = lookupName x context
synthVar _ _ = Nothing

{- Synthesize that the type of a function application to some argument w.r.t.
   some typing context is the output type of that function if you can synthesize
   that the function's type w.r.t. to that context and check that the argument's
   type is the function's input type w.r.t. that context. -}
synthAppl :: Context -> Expr -> Maybe Type
synthAppl context (Appl f arg) = do
  fType <- synthesize context f
  case fType of
    TFun tInput tOutput -> do
      () <- check context arg tInput
      return tOutput
    _ -> Nothing
synthAppl _ _ = Nothing

{- Check that a lambda abstraction has a given function type from some input
   type to some output type w.r.t. some typing context by checking that its body
   has the given output type in that context augmented by its parameter being
   assigned the given input type. -}
checkLam :: Context -> Expr -> Type -> Maybe ()
checkLam context (Lambda param body) (TFun tInput tOutput) =
  check (augment param tInput context) body tOutput
checkLam _ _ _ = Nothing

{- If you can synthesize that an expression in a given context has a given type,
   that suffices as a check of the same. -}
checkBySynth :: Context -> Expr -> Type -> Maybe ()
checkBySynth context expr t = do
  synthedT <- synthesize context expr
  guard (synthedT == t)

{- If an expression has been annotated by the developer as having a specific
   type, synthesize w.r.t. to some typing context that it does have that type if
   you can check that it has that type. -}
synthAnn :: Context -> Expr -> Maybe Type
synthAnn context (Annotate e t) = do
  () <- check context e t
  return t
synthAnn _ _ = Nothing

-- bidirectional

-- checking  -- give me an expression and a type, and I'll say thumbs up or down
check :: Context -> Expr -> Type -> Maybe ()
check context e t =
  checkLam context e t <|> checkBySynth context e t

-- synthesize -- give me an expression, I'll produce a type
synthesize :: Context -> Expr -> Maybe Type
synthesize context e =
  synthInt context e
  <|> synthStr context e
  <|> synthVar context e
  <|> synthAppl context e
  <|> synthAnn context e
