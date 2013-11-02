import Test.Hspec
import Text.ParserCombinators.Parsec (ParseError, parse)

import Hodor


instance Eq ParseError where
  (==) a b = (show a) == (show b)


testParse parser example = parse parser "(unknown)" example

x `shouldBeRight` y = x `shouldBe` (Right y)


main :: IO ()
main = hspec $ do
  describe "hodor.date" $
    it "parses dates" $
      testParse date "2013-12-25 " `shouldBeRight` ("2013", "12", "25")
  describe "hodor.project" $
    it "parses project" $
      testParse project "+foo" `shouldBeRight` Project "foo"
  describe "hodor.context" $
    it "parses context" $
      testParse Hodor.context "@foo" `shouldBeRight` Context "foo"
  describe "hodor.todo line" $ do
    it "parses done items" $ do
      testParse Hodor.line "x 2013-10-12 2013-09-20 A done task +some-project"
        `shouldBeRight` defaultTodoItem { dateCompleted = Just ("2013", "10", "12"),
                                          dateCreated = Just ("2013", "09", "20"),
                                          projects = [Project "some-project"] }
      testParse Hodor.line
        "x 2013-10-12 Something else"
        `shouldBeRight` defaultTodoItem { dateCompleted = Just ("2013", "10", "12") }
    it "parses incomplete items" $ do
      testParse Hodor.line
        "2013-09-21 Email John arranging time to catch up @online +some-project"
        `shouldBeRight` defaultTodoItem { dateCreated = Just ("2013", "09", "21"),
                                          projects = [Project "some-project"],
                                          contexts = [Context "online"] }
      testParse Hodor.line
        "(B) 2013-09-27 Wipe mould off bathroom ceiling +condensation @home"
        `shouldBeRight` defaultTodoItem { dateCreated = Just ("2013", "09", "27"),
                                          projects = [Project "condensation"],
                                          contexts = [Context "home"],
                                          priority = Just 'B' }
