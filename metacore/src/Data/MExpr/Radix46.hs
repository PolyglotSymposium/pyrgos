{-# LANGUAGE ScopedTypeVariables #-}
module Data.MExpr.Radix46 (is46, decode46, encode46) where

import Data.Bits.Utils (c2w8, w82c)
import Data.Word (Word8)

is46 :: Char -> Bool
is46 c = '-' <= c && c <= 'Z'

zero :: Word8
zero = c2w8 '-'

to46 :: Char -> Word8
to46 c = c2w8 c - zero

from46 :: Word8 -> Char
from46 x = w82c (x + zero)

significance46 :: [Integer]
significance46 = 1 : fmap (* 46) significance46

littleEndian :: String -> Integer
littleEndian =
  sum . fmap (uncurry (*)) . zip significance46 . fmap (toInteger . to46)

-- | A radix 46 encoding
decode46 :: forall a. (Bounded a, Integral a) => String -> Maybe a
decode46 s =
  let x = littleEndian s
  in if x > toInteger (maxBound :: a)
     then Nothing
     else Just $ fromInteger x


chars46 :: forall a. Integral a => a -> [Word8]
chars46 x | x < 46 = [fromIntegral x]
chars46 x =
  let (d, m) = divMod x 46
  in fromIntegral m : chars46 d

encode46 :: forall a. Integral a => a -> String
encode46 = fmap from46 . chars46
