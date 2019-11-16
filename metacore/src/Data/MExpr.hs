-- | McCarthy's s-expressions get so much love. Type to hype up his
-- | m-expressions a bit.
module Data.MExpr (MExpr(..)) where

import Data.MExpr.Symbol (Symbol)
import Data.Word (Word64)

data MExpr
  = StrLit String
  | NatLit Integer
  | SymLit Word64
  | ChrLit Char
  | MExpr Symbol [MExpr] -- Not strictly positive...
  deriving Show
