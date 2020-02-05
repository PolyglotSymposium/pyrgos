-- | At the moment, extremely plain symbols.
module Data.MExpr.Symbol (Symbol(..), symbol) where

import Data.MExpr.Radix46 (encode46)
import Data.Word (Word64)

-- | An opaque type representing what a valid symbol is an an m-expression.
-- | Opaque so we can change our mind about the guts of the type in the future.
newtype Symbol = Symbol Word64 deriving Eq

instance Show Symbol where
  show (Symbol w) = encode46 w

symbol :: Word64 -> Symbol
symbol = Symbol
