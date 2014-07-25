module Tests.TypesSpec where

import Data.Time (fromGregorian)
import Test.Hspec

import Hodor.Parser (parseTodoFile)
import Hodor.Types (
  dateCompleted,
  doItems,
  listItems,
  unsafeMakePriority,
  makeTodoFile,
  noPriority,
  TaskAction(..),
  TodoEvent(..),
  unsafeGetItem
  )


spec :: Spec
spec = do
  describe "mark as done" $ do
    let someDay = fromGregorian 1982 12 25
    describe "when there are no todos" $ do
      let emptyFile = makeTodoFile "empty" []
      it "reports no such task" $ do
        snd (doItems someDay emptyFile [2]) `shouldBe` [NoSuchTask 2]
      it "reports no such task for all given tasks" $ do
        snd (doItems someDay emptyFile [2, 3]) `shouldBe` [NoSuchTask 2, NoSuchTask 3]
      it "doesn't create new tasks" $ do
        (listItems $ fst (doItems someDay emptyFile [2, 3])) `shouldBe` []

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
            originalItem = unsafeGetItem sampleTodo index
            (_, events) = doItems someDay sampleTodo [index]
        events `shouldBe` [TaskChanged Done index originalItem originalItem { dateCompleted = Just someDay }]
      it "marks the item as done" $ do
        let index = 2
            originalItem = unsafeGetItem sampleTodo index
            todoWriter = doItems someDay sampleTodo [index]
        unsafeGetItem (fst todoWriter) index `shouldBe` originalItem { dateCompleted = Just someDay }

  describe "priorities" $ do
    describe "ordering" $ do
      it "ranks A above B" $ do
        unsafeMakePriority 'A' < unsafeMakePriority 'B' `shouldBe` True
        unsafeMakePriority 'A' >= unsafeMakePriority 'B' `shouldBe` False
      it "ranks A above D" $ do
        unsafeMakePriority 'A' < unsafeMakePriority 'D' `shouldBe` True
        unsafeMakePriority 'A' >= unsafeMakePriority 'D' `shouldBe` False
      it "ranks any priority above none" $ do
        unsafeMakePriority 'A' < noPriority `shouldBe` True
        unsafeMakePriority 'Z' < noPriority `shouldBe` True
