import Test.Hspec
import Text.ParserCombinators.Parsec (ParseError, parse)

import Hodor (date)

instance Eq ParseError where
  (==) a b = (show a) == (show b)


main :: IO ()
main = hspec $ do
  describe "hodor.date" $ do
    it "parses dates" $ do
      parse date "(unknown)" "2013-12-25 " `shouldBe` Right ("2013", "12", "25")
