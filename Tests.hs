import Data.Time (fromGregorian)
import Test.Hspec
import Text.ParserCombinators.Parsec (ParseError, parse)

import Hodor.Types
import Hodor.Parser


instance Eq ParseError where
  (==) a b = show a == show b


emptyTodoItem :: TodoItem
emptyTodoItem = TodoItem { dateCompleted = Nothing,
                           priority = Nothing,
                           dateCreated = Nothing,
                           projects = [],
                           contexts = [],
                           description = "" }


testParse parser = parse parser "(unknown)"

x `shouldBeRight` y = x `shouldBe` Right y


main :: IO ()
main = hspec $ do
  describe "hodor.todo line" $ do
    it "parses done items" $ do
      let input = "x 2013-10-12 2013-09-20 A done task +some-project"
      testParse todoTxtLine input
        `shouldBeRight` emptyTodoItem { dateCompleted = Just (fromGregorian 2013 10 12),
                                        dateCreated = Just (fromGregorian 2013 9 20),
                                        projects = [Project "some-project"],
                                        description = input }
      let input = "x 2013-10-12 Something else"
      testParse todoTxtLine
        input
        `shouldBeRight` emptyTodoItem { dateCompleted = Just (fromGregorian 2013 10 12),
                                        description = input }
    it "parses incomplete items" $ do
      let input = "2013-09-21 Email John arranging time to catch up @online +some-project"
      testParse todoTxtLine input
        `shouldBeRight` emptyTodoItem { dateCreated = Just (fromGregorian 2013 9 21),
                                        projects = [Project "some-project"],
                                        contexts = [Context "online"],
                                        description = input }
      let input = "(B) 2013-09-27 Wipe mould off bathroom ceiling +condensation @home"
      testParse todoTxtLine input
        `shouldBeRight` emptyTodoItem { dateCreated = Just (fromGregorian 2013 9 27),
                                        projects = [Project "condensation"],
                                        contexts = [Context "home"],
                                        priority = Just 'B',
                                        description = input }
