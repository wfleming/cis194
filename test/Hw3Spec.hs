module Hw3Spec (main, spec) where

import Test.Hspec
import Golf

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "skips" $ do
    it "skips strings" $ do
      skips "ABCD" `shouldBe` ["ABCD", "BD", "C", "D"]
      skips "hello!" `shouldBe` ["hello!", "el!", "l!", "l", "o", "!"]
      skips [1] `shouldBe` [[1]]
      skips [True, False] `shouldBe` [[True, False], [False]]
      skips [] `shouldBe` ([] :: [[Int]])

  describe "localMaxima" $ do
    it "finds local maxima" $ do
      localMaxima [2,9,5,6,1] `shouldBe` [9,6]
      localMaxima [2,3,4,1,5] `shouldBe` [4]
      localMaxima [1,2,3,4,5] `shouldBe` []

  describe "histogram" $ do
    it "creates a histogram string" $ do
      histogram [1,1,1,5] `shouldBe` (unlines [
         " *        ",
         " *        ",
         " *   *    ",
         "==========",
         "0123456789"
        ])
