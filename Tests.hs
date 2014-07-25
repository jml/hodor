module Main where

import Test.Hspec

import qualified Tests.FormatSpec
import qualified Tests.ParserSpec
import qualified Tests.TypesSpec


spec :: Spec
spec = do
  Tests.FormatSpec.spec
  Tests.ParserSpec.spec
  Tests.TypesSpec.spec


main :: IO ()
main = hspec spec
