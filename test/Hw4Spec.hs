module Hw4Spec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Hw4

main :: IO ()
main = hspec spec

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

list1 :: [Integer]
list1 = [1, 2, 3]

isBalanced :: Tree a -> Bool
isBalanced Leaf = True
isBalanced (Node _ l _ r) = abs (height l - height r) <= 1

spec :: Spec
spec = do
  describe "fun1'" $ do
    it "behaves like fun1" $ property $ \is -> do
      fun1 is `shouldBe` fun1' is

  describe "fun2'" $ do
    -- this is hanging
    {-it "behaves like fun2" $ property $ \is -> do
      fun2 is `shouldBe` fun2' is-}
    it "does the right thing in specific cases" $ do
      fun2 10 `shouldBe` fun2' 10
      fun2 7 `shouldBe` fun2' 7

  describe "foldTree" $ do
    it "calculates a tree" $ do
      (height $ foldTree "ABCDEFGHIJ") `shouldBe` 3
      (isBalanced $ foldTree "ABCDEFGHIJ") `shouldBe` True

  describe "xor" $ do
    it "is correct" $ do
      xor [] `shouldBe` False
      xor [True, True] `shouldBe` False
      xor [True, False] `shouldBe` True
      xor [True] `shouldBe` True
      xor [True, False, True, False, True] `shouldBe` True

  describe "map'" $ do
    it "maps" $ do
      map' (*2) list1 `shouldBe` [2, 4, 6]

