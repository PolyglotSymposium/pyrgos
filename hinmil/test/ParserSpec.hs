--{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ParserSpec (spec) where

--import Control.Applicative hiding (some)
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec
import Parser (name, equals)

spec :: Spec
spec = do
    it "should parse a name" $
      parse name "" "bob" `shouldParse` "bob" 
    it "should parse equal signs" $
      parse equals "" "=" `shouldParse` ()
