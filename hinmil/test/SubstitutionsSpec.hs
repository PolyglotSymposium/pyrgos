{-# OPTIONS_GHC -fno-warn-orphans #-}
module SubstitutionsSpec where

import Test.Hspec
import Test.QuickCheck

import TypeAST
import Substitutions
import Data.List.NonEmpty (NonEmpty(..))

decr :: Int -> Int
decr x = x - 1

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
  frequency [ (originalSize, arbTyVar)
            , (size, arbTyApp originalSize)
            ]

instance Arbitrary Term where
  arbitrary = resize 5 $ do
    size <- getSize
    arbTerm size

instance Arbitrary Substitution where
  arbitrary = suchThatMap genMaybe id where
    genMaybe = do
      c <- arbitraryPrintableChar
      PrintableString name <- arbitrary
      term <- arbitrary
      return $ subst (c : name) term

instance Arbitrary Substitutions where
  arbitrary = foldMap sub1 <$> listOf arbitrary

associativity :: Substitutions -> Substitutions -> Substitutions -> Bool
associativity a b c =
  let ab = a <> b
      bc = b <> c
  in ab <> c == a <> bc

spec :: Spec
spec = do
  it "compose associatively" $ property associativity
