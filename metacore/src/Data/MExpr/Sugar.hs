{-# LANGUAGE ScopedTypeVariables #-}
module Data.MExpr.Sugar (is46, decode46) where

import Data.Word (Word8)

-- | From MissingH; remove if dependency on MissingH gets added
c2w8 :: Char -> Word8
c2w8 = fromIntegral . fromEnum

is46 :: Char -> Bool
is46 c = '-' <= c && c <= 'Z'

zero :: Word8
zero = c2w8 '-'

to46 :: Char -> Word8
to46 c = c2w8 c - zero

significance46 :: [Integer]
significance46 = 1 : fmap (* 46) significance46

littleEndian :: [Char] -> Integer
littleEndian =
  sum . fmap (\(x, y) -> x * y) . zip significance46 . fmap (toInteger . to46)

-- | A radix 46 encoding
decode46 :: forall a. (Bounded a, Integral a) => String -> Maybe a
decode46 s =
  let x = littleEndian s
  in if x > toInteger (maxBound :: a)
     then Nothing
     else Just $ fromInteger x
