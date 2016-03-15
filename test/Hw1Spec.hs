module Hw1Spec (main, spec) where

import Test.Hspec
import Hw1

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "lastDigit" $ do
    it "gets the last digit of a number" $ do
      lastDigit 12345 `shouldBe` 5

  describe "dropLastDigit" $ do
    it "drops the last digit of a number" $ do
      dropLastDigit 12345 `shouldBe` 1234

  describe "toDigits" $ do
    it "breaks a number into digits" $ do
      toDigits 12345 `shouldBe` [1, 2, 3, 4, 5]

  describe "doubleEveryOther" $ do
    it "double odd indices" $ do
      doubleEveryOther [4, 9, 5, 5] `shouldBe` [4, 18, 5, 10]

  describe "sumDigits" $ do
    it "sums the digits" $ do
      sumDigits [4, 9, 5] `shouldBe` (4 + 9 + 5)

  describe "validate" $ do
    it "should be true for a valid CC number" $ do
      validate 4012888888881881 `shouldBe` True

    it "should be false for an invalid CC number" $ do
      validate 4012888888881882 `shouldBe` False

  describe "hanoi" $ do
    it "should be the right sequence of moves for 2 discs" $ do
      hanoi 2 "a" "b" "c" `shouldBe` [("a","c"), ("a","b"), ("c","b")]

    it "should be the right sequence of moves for 3 discs" $ do
      hanoi 3 "a" "b" "c" `shouldBe` [("a","b"), ("a","c"), ("b","c"), ("a","b"), ("c","a"), ("c","b"), ("a","b")]
