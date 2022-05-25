{-# OPTIONS_GHC -fno-warn-orphans #-}
module SubstitutionsSpec (spec) where

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

nonCommutativity :: Substitutions -> Substitutions -> Bool
nonCommutativity a b =
  let ab = a <> b
      ba = b <> a
  in ab /= ba || a == b || a == mempty || b == mempty

spec :: Spec
spec = do
  it "compose associatively" $ property associativity
  it "composition does not commute for distinct non-empty values" $ property nonCommutativity
