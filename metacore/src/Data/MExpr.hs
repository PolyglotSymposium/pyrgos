-- | McCarthy's s-expressions get so much love. Time to hype up his
-- | m-expressions a bit.
module Data.MExpr
  ( MExpr(..), Emify(..), Deemify(..)
  ) where

import Data.List.Utils (replace)
import Data.List (intercalate)
import Data.MExpr.Symbol (Symbol)
import Data.Word (Word64)
import Data.MExpr.Radix46 (encode46)

data MExpr             =
  ChrLit Char          |
  MExpr Symbol [MExpr] | -- Not strictly positive...
  NatLit Integer       |
  StrLit String        |
  SymLit Word64        --

class Emify a where
  emify :: a -> MExpr

class Deemify a where
  deemify :: MExpr -> Maybe a

instance Show MExpr where
  show = \case
    StrLit s -> '"' : replace "\"" "\\\"" s ++ "\""
    NatLit n -> show n
    SymLit s -> '#' : encode46 s
    ChrLit c -> ['\'', c, '\'']
    MExpr s ms ->
      let xs = fmap show ms
      in show s ++ "(" ++ intercalate ", " xs ++ ")"
