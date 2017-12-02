module Day01Spec where

import Control.Exception (evaluate)
import qualified Day01.Part1 as Part1
import qualified Day01.Part2 as Part2
import Test.Hspec

spec =
  describe "Specs" $ do
    describe "Part1" $ do
      it "1122" $ do Part1.solution "1122" `shouldBe` (3 :: Integer)
      it "1111" $ do Part1.solution "1111" `shouldBe` (4 :: Integer)
      it "1234" $ do Part1.solution "1234" `shouldBe` (0 :: Integer)
      it "91212129" $ do Part1.solution "91212129" `shouldBe` (9 :: Integer)
    describe "Part2" $ do
      it "1212" $ do Part2.solution "1212" `shouldBe` (6 :: Integer)
      it "1221" $ do Part2.solution "1221" `shouldBe` (0 :: Integer)
      it "123425" $ do Part2.solution "123425" `shouldBe` (4 :: Integer)
      it "123123" $ do Part2.solution "123123" `shouldBe` (12 :: Integer)
      it "12131415" $ do Part2.solution "12131415" `shouldBe` (4 :: Integer)
