module Hw2Spec (main, spec) where

import Test.Hspec
import Log
import LogAnalysis

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
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
