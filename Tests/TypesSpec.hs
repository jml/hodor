module Tests.TypesSpec (spec) where

import Data.Char (isAlpha, toUpper)
import Data.Time (fromGregorian)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import Hodor.Parser (parseTodoFile)
import Hodor.Types (
  dateCompleted,
  doItems,
  isDone,
  hasPriority,
  listItems,
  makePriority,
  makeTodoFile,
  markAsDone,
  markAsUndone,
  noPriority,
  prioritize,
  TaskAction(..),
  TodoEvent(..),
  TodoFile,
  unsafeGetItem,
  unsafeMakePriority
  )

import Tests.Generators


emptyFile :: TodoFile
emptyFile = makeTodoFile "empty" []

sampleTodoText :: String
sampleTodoText = unlines [
  "x 2013-10-12 2013-09-20 A done task +some-project",
  "(B) 2013-09-27 Wipe mould off bathroom ceiling +condensation @home\n",
  "2013-09-21 Email John arranging time to catch up @online +some-project"]

sampleTodo :: TodoFile
sampleTodo = case parseTodoFile "test-todo" sampleTodoText of
  Left e -> error (show e)
  Right r -> r


spec :: Spec
spec = describe "Core operations on todos" $ do
  describe "mark as done" $ do
    let someDay = fromGregorian 1982 12 25
    describe "when there are no todos" $ do
      it "reports no such task" $ do
        snd (doItems someDay emptyFile [2]) `shouldBe` [NoSuchTask 2]
      it "reports no such task for all given tasks" $ do
        snd (doItems someDay emptyFile [2, 3]) `shouldBe` [NoSuchTask 2, NoSuchTask 3]
      it "doesn't create new tasks" $ do
        (listItems $ fst (doItems someDay emptyFile [2, 3])) `shouldBe` []

    describe "with todos" $ do
      it "reports that it marks item as done" $ do
        let index = 2
            originalItem = unsafeGetItem sampleTodo index
            (_, events) = doItems someDay sampleTodo [index]
        events `shouldBe` [TaskChanged Done index originalItem originalItem { dateCompleted = Just someDay }]
      it "marks the item as done" $ do
        let index = 2
            originalItem = unsafeGetItem sampleTodo index
            todoWriter = doItems someDay sampleTodo [index]
        unsafeGetItem (fst todoWriter) index `shouldBe` originalItem { dateCompleted = Just someDay }

  describe "priorities" $ do
    describe "x < y means x has higher priority than y" $ do
      prop "earlier characters have higher priority (A) < (Z)" $
        forAll arbitraryPriorityLetter $ \a ->
        forAll arbitraryPriorityLetter $ \b -> a < b ==> unsafeMakePriority a < unsafeMakePriority b
      prop "later characters have lower priority (Z) > (A)" $
        forAll arbitraryPriorityLetter $ \a ->
        forAll arbitraryPriorityLetter $ \b -> a > b ==> unsafeMakePriority a > unsafeMakePriority b
      prop "no priority is the lowest priority" $
        forAll arbitraryPriority $ \p -> p < noPriority

    describe "makePriority" $ do
      prop "converts lower-case" $
        forAll (choose ('a', 'z')) $ \x -> makePriority x == makePriority (toUpper x)

      prop "creates the same as unsafeMakePriority" $
        forAll arbitraryPriorityLetter $ \x -> makePriority x == Just (unsafeMakePriority x)

      prop "returns Nothing for non-priorities" $
        \x -> not (isAlpha x) ==> makePriority x == Nothing

  describe "marking items as done" $ do
    prop "markAsDone implies isDone" $
      \date item -> isDone (markAsDone date item)
    prop "markAsDone sets date completed if not done" $
      \date item -> not (isDone item) ==> dateCompleted (markAsDone item date) == Just date
    prop "markAsDone leaves date completed if already done" $
      \date item -> isDone item ==> dateCompleted (markAsDone item date) == (dateCompleted item)
    prop "markAsUndone implies not done" $
      not . isDone . markAsUndone

  describe "setting priorities" $ do
    prop "setting a priority shoes there *is* a priority" $
      forAll arbitraryPriority $ \pri ->
      \item -> hasPriority (prioritize item pri)
    prop "setting no priority shows there is not a priority" $
      \item -> not . hasPriority $ prioritize item noPriority
