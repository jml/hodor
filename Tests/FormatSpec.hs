module Tests.FormatSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck

import Hodor.Format
import Hodor.Types


spec :: Spec
spec = describe "Formatting user output" $ do
  describe "appMessage" $ do
    prop "returns a string prefixed by the appName" $
      \x -> appMessage x == appName ++ ": " ++ x
  describe "format" $ do
    describe "priority" $ do
      it "shows priorities as (N)" $ format (unsafeMakePriority 'A') `shouldBe` "(A)"
      it "shows no priority as warning message" $ format noPriority `shouldBe` "<<no priority>>"
