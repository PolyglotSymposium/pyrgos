-- | McCarthy's s-expressions get so much love. Type to hype up his
-- | m-expressions a bit.
module Data.MExpr where

import Data.MExpr.Symbol (Symbol)

data MExpr
  = Literal String
  | MExpr Symbol [MExpr] -- Not strictly positive...
  deriving Show
