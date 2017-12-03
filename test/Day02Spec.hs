module Day02Spec where

import Control.Exception (evaluate)
import qualified Day02.Part1 as Part1
import qualified Day02.Part2 as Part2
import Test.Hspec

inputPart1 = [[5, 1, 9, 5], [7, 5, 3], [2, 4, 6, 8]]

inputPart2 = [[5, 9, 2, 8], [9, 4, 7, 3], [3, 8, 6, 5]]

spec =
  describe "Specs" $ do
    describe "Part1" $ do
      it (show inputPart1) $ do
        Part1.solution inputPart1 `shouldBe` (18 :: Integer)
    describe "Part2" $ do
      it (show inputPart2) $ do
        Part2.solution inputPart2 `shouldBe` (9 :: Integer)
