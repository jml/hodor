module Tests.ParserSpec where

import Control.Monad (liftM)
import Data.Time (fromGregorian)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck hiding (output)

import Hodor.Functional (onLeft)
import Hodor.Parser (
  parseTodoFile,
  parseTodoLine,
  )
import Hodor.Types

import Tests.Generators


testParse = onLeft show . parseTodoLine


shouldHave :: (Monad m, Show (m b), Eq (m b)) => m a -> (a -> b) -> b -> Expectation
shouldHave x p y = (liftM p x) `shouldBe` (return y)


lineParserSpec :: Spec
lineParserSpec =
  describe "hodor.todo line" $ do
    describe "parses done items" $ do
      let input = "x 2013-10-12 2013-09-20 A done task +some-project"
          parsed = testParse input
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
          parsed = testParse input
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
          parsed = testParse input
      it "extracts date created" $ do
        shouldHave parsed dateCreated (Just (fromGregorian 2013 9 27))
      it "extracts projects" $ do
        shouldHave parsed projects [Project "condensation"]
      it "extracts contexts" $ do
        shouldHave parsed contexts [Context "home"]
      it "extracts priority" $ do
        shouldHave parsed itemPriority (unsafeMakePriority 'B')
      it "can be described" $ do
        shouldHave parsed unparse input

    describe "simple line" $ do
      let input = "Do a thing"
          parsed = testParse input
      it "can be described" $ do
        shouldHave parsed unparse input


fileParserSpec :: Spec
fileParserSpec =
  describe "hodor.todo file" $ do
    describe "empty file" $ do
      let input = ""
          output = onLeft show (parseTodoFile "test-todo" input)
      it "has no items" $ do
        shouldHave output listItems []
      it "has the given name" $ do
        shouldHave output todoFileName "test-todo"
    describe "blank lines" $ do
      it "are skipped" $ do
        let input = "\n\n"
            output = onLeft show (parseTodoFile "test-todo" input)
        shouldHave output listItems []
    describe "single line" $ do
      it "is parsed" $ do
        let input = "(B) 2013-09-27 Wipe mould off bathroom ceiling +condensation @home\n"
            output = onLeft show (parseTodoFile "test-todo" input)
        fmap listItems output `shouldBe` fmap (\x -> [(1, x)]) (testParse input)


unparseSpec :: Spec
unparseSpec = describe "unparsing todo items" $ do
  describe "priorities" $ do
    it "no priority is empty string" $
      unparse noPriority `shouldBe` ""
    prop "priority is '(A) '" $
      forAll arbitraryPriorityLetter $ \x -> unparse (unsafeMakePriority x) `shouldBe` '(':x:')':' ':[]
  describe "reverses parsing for lines" $ do
    prop "parse then unparse" $
      forAll arbitraryTodoLines $ \x -> Right x `shouldBe` fmap unparse (testParse x)
    prop "unparse then parse" $
      \x -> testParse (unparse x) `shouldBe` Right x
  prop "reverses parsing for files" $
    \x -> onLeft show (parseTodoFile (todoFileName x) (unparse x)) `shouldBe` Right x


spec :: Spec
spec = describe "Parser" $ do
  lineParserSpec
  fileParserSpec
  unparseSpec
