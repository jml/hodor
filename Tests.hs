module Main where

import Control.Monad.Writer
import Data.Time (fromGregorian)
import Test.Hspec
import Text.ParserCombinators.Parsec (parse)

import Hodor.Functional (onLeft)
import Hodor.Types (
  Context(Context),
  DoneResult(..),
  Project(Project),
  contexts,
  dateCompleted,
  dateCreated,
  description,
  doItems,
  priority,
  projects,
  TodoFile(..),
  unparse,
  )
import Hodor.Parser (
  ParseError(ParseError),
  parseTodoFile,
  todoTxtLine,
  )


testParse parser = onLeft ParseError . parse parser "(unknown)"

shouldHave x p y = (fmap p x) `shouldBe` (return y)


main :: IO ()
main = hspec $ do
  describe "hodor.todo line" $ do
    describe "parses done items" $ do
      let input = "x 2013-10-12 2013-09-20 A done task +some-project"
          parsed = testParse todoTxtLine input
      it "extracts date completed" $ do
        shouldHave parsed dateCompleted (Just (fromGregorian 2013 10 12))
      it "extracts date created" $ do
        shouldHave parsed dateCreated (Just (fromGregorian 2013 9 20))
      it "has a project" $ do
        shouldHave parsed projects [Project "some-project"]
      it "can be described" $ do
        shouldHave parsed description "A done task +some-project"
      it "can be described" $ do
        shouldHave parsed unparse input

    describe "parses incomplete items" $ do
      let input = "2013-09-21 Email John arranging time to catch up @online +some-project"
          parsed = testParse todoTxtLine input
      it "extracts date created" $ do
        shouldHave parsed dateCreated (Just (fromGregorian 2013 9 21))
      it "extracts projects" $ do
        shouldHave parsed projects [Project "some-project"]
      it "extracts contexts" $ do
        shouldHave parsed contexts [Context "online"]
      it "can be described" $ do
        shouldHave parsed unparse input

    describe "parses complicated incomplete items" $ do
      let input = "(B) 2013-09-27 Wipe mould off bathroom ceiling +condensation @home"
          parsed = testParse todoTxtLine input
      it "extracts date created" $ do
        shouldHave parsed dateCreated (Just (fromGregorian 2013 9 27))
      it "extracts projects" $ do
        shouldHave parsed projects [Project "condensation"]
      it "extracts contexts" $ do
        shouldHave parsed contexts [Context "home"]
      it "extracts priority" $ do
        shouldHave parsed priority (Just 'B')
      it "can be described" $ do
        shouldHave parsed unparse input

    describe "simple line" $ do
      let input = "Do a thing"
          parsed = testParse todoTxtLine input
      it "can be described" $ do
        shouldHave parsed unparse input

  describe "hodor.todo file" $ do
    describe "empty file" $ do
      let input = ""
          output = parseTodoFile "test-todo" input
      it "has no items" $ do
        shouldHave output todoFileItems []
      it "has the given name" $ do
        shouldHave output todoFileName "test-todo"
    describe "blank lines" $ do
      it "are skipped" $ do
        let input = "\n\n"
            output = parseTodoFile "test-todo" input
        shouldHave output todoFileItems []
    describe "single line" $ do
      it "is parsed" $ do
        let input = "(B) 2013-09-27 Wipe mould off bathroom ceiling +condensation @home\n"
            output = parseTodoFile "test-todo" input
        fmap todoFileItems output `shouldBe` fmap (\x -> [x]) (testParse todoTxtLine input)

  describe "mark as done" $ do
    let someDay = fromGregorian 1982 12 25
    describe "when there are no todos" $ do
      let emptyFile = TodoFile { todoFileName = "empty", todoFileItems = [] }
      it "reports no such task" $ do
        (execWriter $ doItems emptyFile someDay [2]) `shouldBe` [NoSuchTask 2]
      it "reports no such task for all given tasks" $ do
        (execWriter $ doItems emptyFile someDay [2, 3]) `shouldBe` [NoSuchTask 2, NoSuchTask 3]
      it "doesn't create new tasks" $ do
        (todoFileItems $ fst $ runWriter (doItems emptyFile someDay [2, 3])) `shouldBe` []

    describe "with todos" $ do
      let sampleTodoText = unlines [
            "x 2013-10-12 2013-09-20 A done task +some-project",
            "(B) 2013-09-27 Wipe mould off bathroom ceiling +condensation @home\n",
            "2013-09-21 Email John arranging time to catch up @online +some-project"]
          sampleTodo = case parseTodoFile "test-todo" sampleTodoText of
            Left e -> error (show e)
            Right r -> r
      it "reports that it marks item as done" $ do
        let index = 2
            originalItem = todoFileItems sampleTodo !! (index - 1)
            todoWriter = doItems sampleTodo someDay [index]
        (execWriter todoWriter) `shouldBe` [Done index originalItem { dateCompleted = Just someDay } ]
      it "marks the item as done" $ do
        let index = 2
            originalItem = todoFileItems sampleTodo !! (index - 1)
            todoWriter = doItems sampleTodo someDay [index]
        (todoFileItems $ fst $ runWriter todoWriter) !! (index - 1) `shouldBe` originalItem { dateCompleted = Just someDay }
