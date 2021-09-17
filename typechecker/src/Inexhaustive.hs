{-# LANGUAGE TypeApplications #-}
module Inexhaustive
  ( Inexhaustive, inexhaustive
  , runToMaybe, isDefinedAt
  ) where

import Prelude hiding ((.), id)
import Control.Applicative
import Control.Exception
import Control.Category
import Data.Maybe
import System.IO.Unsafe (unsafePerformIO)

newtype Inexhaustive a b = Inexhaustive (a -> b)

inexhaustive :: (a -> b) -> Inexhaustive a b
inexhaustive = Inexhaustive

runToMaybe :: Inexhaustive a b -> a -> Maybe b
runToMaybe (Inexhaustive f) x =
  let io = fmap Just $ evaluate $ f x
  in unsafePerformIO $ catch @PatternMatchFail io (\_ -> return Nothing)

isDefinedAt :: Inexhaustive a b -> a -> Bool
isDefinedAt f = isJust . runToMaybe f

instance Functor (Inexhaustive z) where
  fmap f (Inexhaustive g) = inexhaustive (f . g)

instance Applicative (Inexhaustive z) where
  pure x = inexhaustive (const x)
  (Inexhaustive zab) <*> (Inexhaustive za) = Inexhaustive (\z -> zab z $ za z)

instance Alternative (Inexhaustive z) where
  empty = inexhaustive (\_ -> throw $ PatternMatchFail "inexhaustive empty")
  f <|> g =
    inexhaustive $ \x ->
      case runToMaybe f x <|> runToMaybe g x of
        Just y -> y

instance Category Inexhaustive where
  id = inexhaustive id
  (Inexhaustive f) . (Inexhaustive g) = Inexhaustive (f . g)
