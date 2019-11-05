-- | At the moment, extremely plain symbols.
module Data.MExpr.Symbol (Symbol, toSymbol, isSymbolChar, avowSymbol) where

import Data.Char (isAlpha)
import Control.Monad (guard)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NEL

-- | An opaque type representing what a valid symbol is an an m-expression.
-- | Exposed at the level of a whole symbol rather than say a nonempty list of
-- | opaque symbol characters, because if we wanted to allow say a0 but not 0a
-- | then it's not a simple free monoid.
newtype Symbol = Symbol (NonEmpty Char)

instance Show Symbol where
  show (Symbol cs) = NEL.toList cs

isSymbolChar :: Char -> Bool
isSymbolChar = isAlpha -- Start simple... maybe stay simple

toSymbolChar :: Char -> Maybe Char
toSymbolChar c = do
  guard (isSymbolChar c)
  return c

toSymbol :: String -> Maybe Symbol
toSymbol cs = do
  cs' <- traverse toSymbolChar cs
  cs'' <- NEL.nonEmpty cs'
  return $ Symbol cs''

-- | Unsafe on multiple levels!
avowSymbol :: [Char] -> Symbol
avowSymbol = Symbol . NEL.fromList
