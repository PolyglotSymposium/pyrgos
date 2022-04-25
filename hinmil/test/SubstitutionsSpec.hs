{-# OPTIONS_GHC -fno-warn-orphans #-}
module SubstitutionsSpec where

import Arbitraries.TypeAST ()
import Test.Hspec
import Test.QuickCheck

import Substitutions

-- TODO failed with seed 1815262216

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
