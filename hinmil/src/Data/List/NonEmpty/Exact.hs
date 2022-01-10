module Data.List.NonEmpty.Exact (zipExactNEL) where

import Data.List.Exact
import Data.List.NonEmpty (NonEmpty(..))

zipExactNEL :: NonEmpty a -> NonEmpty b -> Maybe (NonEmpty (a, b))
zipExactNEL (x :| xs) (y :| ys) = ((x, y) :|) <$> zipExact xs ys
