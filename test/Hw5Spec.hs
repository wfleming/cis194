module Hw5Spec (main, spec) where

import Test.Hspec
import ExprT
import Calc

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "eval" $ do
    it "evaluates simple expressions" $ do
      eval (Lit 42) `shouldBe` 42
      eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) `shouldBe` 20

  describe "evalStr" $ do
    it "evaluates simple expressions" $ do
      evalStr "42" `shouldBe` Just 42
      evalStr "(2 + 3) * 4" `shouldBe` Just 20

    it "handles unparseable expressions" $ do
      evalStr "foo" `shouldBe` Nothing
      evalStr "13 +" `shouldBe` Nothing

  describe "Expr Integer instance" $ do
    it "does math" $ do
      lit 42 `shouldBe` (42 :: Integer)
      add 10 2 `shouldBe` (12 :: Integer)
      mul 10 2 `shouldBe` (20 :: Integer)

  describe "Expr Bool instance" $ do
    it "does logic" $ do
      lit 1 `shouldBe` (True :: Bool)
      lit (-1) `shouldBe` (False :: Bool)
      add (lit (-1)) (lit 1) `shouldBe` (True :: Bool)
      add (lit (-1)) (lit (-1)) `shouldBe` (False :: Bool)
      mul (lit 10) (lit 2) `shouldBe` (True :: Bool)
      mul (lit 10) (lit (-2)) `shouldBe` (False :: Bool)

  describe "Expr MinMax instance" $ do
    it "does min maxing" $ do
      lit 1 `shouldBe` (MinMax 1)
      add (lit 10) (lit 5) `shouldBe` (MinMax 10)
      mul (lit 10) (lit 5) `shouldBe` (MinMax 5)

  describe "Expr Mod7 instance" $ do
    it "does math" $ do
      lit 6 `shouldBe` (Mod7 6)
      lit 8 `shouldBe` (Mod7 1)
      add (lit 4) (lit 3) `shouldBe` (Mod7 0)
      mul (lit 10) (lit 2) `shouldBe` (Mod7 6)
