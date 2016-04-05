module Hw2Spec (main, spec) where

import Test.Hspec
import Log
import LogAnalysis

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "LogMessage" $ do
    describe "parseMessage" $ do
      it "parses an error message" $ do
        parseMessage "E 2 562 help help" `shouldBe` LogMessage (Error 2) 562 "help help"

      it "parses an info message" $ do
        parseMessage "I 29 la la la" `shouldBe` LogMessage Info 29 "la la la"

      it "returns Unkown when message is not valid" $ do
        parseMessage "This is not in the right format" `shouldBe` Unknown "This is not in the right format"

    describe "parse" $ do
      it "parses multiple messages" $ do
        parse "E 2 562 help help\nI 29 la la la" `shouldBe` [
            (LogMessage (Error 2) 562 "help help"),
            (LogMessage Info 29 "la la la")]

  describe "MessageTree" $ do
    describe "insert" $ do
      it "inserts into a leaf" $ do
        let msg = LogMessage Info 29 "foo"
        insert msg Leaf `shouldBe` Node Leaf msg Leaf

      it "sorts correctly" $ do
        let msg0 = LogMessage Info 29 "foo"
        let msg1 = LogMessage Info 40 "bar"
        let tree = Node Leaf msg0 Leaf
        insert msg1 tree `shouldBe` Node Leaf msg0 (Node Leaf msg1 Leaf)

    describe "build" $ do
      it "builds" $ do
        let lms = [(LogMessage Info 29 "foo"), (LogMessage Info 40 "bar")]
        build lms `shouldBe` Node (Node Leaf (head lms) Leaf) (head (tail lms)) Leaf

    describe "inOrder" $ do
      it "orders stuff" $ do
        let lms = [(LogMessage Info 29 "foo"), (LogMessage Info 40 "bar")]
        inOrder (build lms) `shouldBe` lms

