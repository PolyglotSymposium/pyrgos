module Disjoint where

import Control.Applicative
import Control.Monad
import Data.Functor
import Syntax

data Disjoint = Disjoint [RawType]

-- data RawType             =
--   TSymbol                |
--   TKeyword Keyword       |
--   TCons RawType RawType  |
--   TNil                   --

disjoint :: RawType -> RawType -> Maybe ()
disjoint TNil TNil = Nothing
disjoint TSymbol TSymbol = Nothing
disjoint (TKeyword x) (TKeyword y) = guard (x /= y)
disjoint (TCons xa xb) (TCons ya yb) =
  guard (xa /= ya) <|> guard (xb /= yb)
disjoint _ _ = Some ()

wellFormed :: Disjoint -> Maybe ()
wellFormed (Disjoint tys) = foldM_ disjointComparedTo tys tys where
  disjointComparedTo :: [RawType] -> RawType -> Maybe [RawType]
  disjointComparedTo others point =
    let result = foldMap (disjoint point) others
        others' =
          case others of
            [] -> []
            _ : xs -> xs
    in result $> others'
