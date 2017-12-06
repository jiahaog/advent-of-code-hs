module Day04Spec where

import Control.Exception (evaluate)
import qualified Day04.Part1 as Part1

import qualified Day04.Part2 as Part2
import Test.Hspec

spec =
  describe "Specs" $ do
    describe "Part1" $ do
      it "aa bb cc dd" $ do
        Part1.solution ["aa", "bb", "cc", "dd"] `shouldBe` True
      it "aa bb cc dd aa" $ do
        Part1.solution ["aa", "bb", "cc", "dd", "aa"] `shouldBe` False
      it "aa bb cc dd aaa" $ do
        Part1.solution ["aa", "bb", "cc", "dd", "aaa"] `shouldBe` True
    describe "Part2" $ do
      it "abcde fghi" $ do Part2.solution ["abcde", "fghi"] `shouldBe` True
      it "abcde xyz ecdab" $ do
        Part2.solution ["abcde", "xyz", "ecdab"] `shouldBe` False
      it "a ab abc abd abf abj" $ do
        Part2.solution ["a", "ab", "abc", "abd", "abf", "abj"] `shouldBe` True
      it "iiii oiii ooii oooi oooo" $ do
        Part2.solution ["iiii", "oiii", "ooii", "oooi", "oooo"] `shouldBe` True
      it "oiii ioii iioi iiio" $ do
        Part2.solution ["oiii", "ioii", "iioi", "iiio"] `shouldBe` False
