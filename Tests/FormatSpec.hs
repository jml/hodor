module Tests.FormatSpec (spec) where

import Test.Hspec

import Hodor.Format


spec :: Spec
spec = describe "Formatting user output" $ do
  describe "appMessage" $ do
    it "returns a string prefixed by the appName" $ do
      appMessage "hello" `shouldBe` appName ++ ": " ++ "hello"
