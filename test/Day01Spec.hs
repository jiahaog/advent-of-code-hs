module Day01Spec where

import Control.Exception (evaluate)
import Day01.Solution (solution)
import Test.Hspec

spec =
  describe "Day01" $ do
    it "1122" $ do solution "1122" `shouldBe` (3 :: Integer)
    it "1111" $ do solution "1111" `shouldBe` (4 :: Integer)
    it "1234" $ do solution "1234" `shouldBe` (0 :: Integer)
    it "91212129" $ do solution "91212129" `shouldBe` (9 :: Integer)
