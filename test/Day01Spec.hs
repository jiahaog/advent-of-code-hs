module Day01Spec where

import Control.Exception (evaluate)
import Day01.Solution (solution)
import Test.Hspec
import Test.QuickCheck

spec =
  describe "Day01" $ do
    it "returns the first element of a list" $ do
      solution "124" `shouldBe` (123 :: Integer)
