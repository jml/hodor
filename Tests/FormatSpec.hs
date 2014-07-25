module Tests.FormatSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck

import Hodor.Format


spec :: Spec
spec = describe "Formatting user output" $ do
  describe "appMessage" $ do
    prop "returns a string prefixed by the appName" $
      \x -> appMessage x == appName ++ ": " ++ x
