{-# OPTIONS_GHC -fno-warn-orphans #-}
module Arbitraries.TypeAST () where

import Data.List.NonEmpty (NonEmpty(..))
import Test.QuickCheck

import TypeAST

decr :: Int -> Int
decr x = x - 1

arbTyLit :: Gen Term
arbTyLit = do
  c <- arbitraryPrintableChar
  PrintableString x <- arbitrary
  return $ TyLit (c : x)

arbTyVar :: Gen Term
arbTyVar = do
  c <- arbitraryPrintableChar
  PrintableString x <- arbitrary
  return $ TyVar (c : x)

arbTyApp :: Int -> Gen Term
arbTyApp originalSize = do
  c <- arbitraryPrintableChar
  PrintableString name <- arbitrary
  term <- scale decr $ arbTerm originalSize
  terms <- listOf $ scale decr $ arbTerm originalSize
  return $ TyApp (c : name) (term :| terms)

arbTerm :: Int -> Gen Term
arbTerm originalSize = do
  size <- getSize
  frequency [ (originalSize, arbTyLit)
            , (originalSize, arbTyVar)
            , (size, arbTyApp originalSize)
            ]

instance Arbitrary Term where
  arbitrary = resize 4 $ do
    size <- getSize
    arbTerm size
