module Tests.FormatSpec (spec) where

import Test.QuickCheck
import Test.Hspec
import Test.Hspec.QuickCheck (prop)

import Hodor.Format
import Hodor.Types
import Tests.Generators


spec :: Spec
spec = describe "Formatting user output" $ do
  describe "appMessage" $ do
    prop "returns a string prefixed by the appName" $
      \x -> appMessage x == appName ++ ": " ++ x

  describe "format" $ do
    describe "priority" $ do
      prop "shows priorities as (N)" $
        forAll arbitraryPriority (\p@(Pri c) -> format p == '(':c:')':[])
