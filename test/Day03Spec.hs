module Day03Spec where

import Control.Exception (evaluate)
import qualified Day03.Part1 as Part1
import Test.Hspec

spec =
  describe "Specs" $ do
    describe "inBoard" $ do
      it "when in board" $ do Part1.inBoard 3 (-1, 1) `shouldBe` True
      it "when in board" $ do Part1.inBoard 5 (-2, 2) `shouldBe` True
      it "when not in board" $ do Part1.inBoard 5 (-2, 3) `shouldBe` False
    describe "addDir" $ do
      it "add right" $ do Part1.addDir 0 (5, 5) `shouldBe` (6, 5)
      it "add up" $ do Part1.addDir 1 (5, 5) `shouldBe` (5, 6)
      it "add left" $ do Part1.addDir 2 (5, 5) `shouldBe` (4, 5)
      it "add down" $ do Part1.addDir 3 (5, 5) `shouldBe` (5, 4)
      it "add right exceed" $ do Part1.addDir 4 (5, 5) `shouldBe` (6, 5)
    describe "solution" $ do
      it "1" $ do Part1.solution 1 `shouldBe` 0
      it "12" $ do Part1.solution 12 `shouldBe` 3
      it "23" $ do Part1.solution 23 `shouldBe` 2
      it "1024" $ do Part1.solution 1024 `shouldBe` 31
