module Data.List.Exact (zipExact) where

zipExact :: [a] -> [b] -> Maybe [(a, b)]
zipExact (x : xs) (y : ys) = ((x, y) :) <$> zipExact xs ys
zipExact [] [] = Just []
zipExact _ _ = Nothing
