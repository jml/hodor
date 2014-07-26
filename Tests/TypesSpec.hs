module Tests.TypesSpec (spec) where

import Data.Char (isAlpha, toUpper)
import Data.Maybe (fromJust)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import Hodor.Types (
  allItems,
  archive,
  dateCompleted,
  doItems,
  isDone,
  hasPriority,
  filterItems,
  getItem,
  isEmpty,
  listItems,
  makePriority,
  makeTodoFile,
  markAsDone,
  markAsUndone,
  noPriority,
  numItems,
  prioritize,
  replaceItem,
  TaskAction(..),
  TodoEvent(..),
  TodoFile,
  unsafeGetItem,
  unsafeMakePriority
  )

import Tests.Generators


emptyFile :: TodoFile
emptyFile = makeTodoFile "empty" []


indexesOf :: TodoFile -> Gen Int
indexesOf file = choose (1, numItems file)

invalidIndexesOf :: TodoFile -> Gen Int
invalidIndexesOf file = arbitrary `suchThat` \i -> i < 1 || i > numItems file


todoFileIndexes :: Gen (TodoFile, Int)
todoFileIndexes = do
  file <- nonEmptyTodoFile
  index <- indexesOf file
  return (file, index)


actionsSpec :: Spec
actionsSpec = describe "High-level operations on todos" $ do
  describe "mark as done" $ do
    describe "for empty files" $ do
      prop "leaves file unchanged and reports NoSuchTask for all given items" $
        \day items -> doItems day emptyFile items `shouldBe` (emptyFile, map NoSuchTask items)

    describe "for invalid items" $ do
      prop "leaves file unchanged and reports NoSuchTask for all given items" $
        \day file -> forAll (listOf $ invalidIndexesOf file) $ \items ->
        doItems day file items `shouldBe` (file, map NoSuchTask items)

    describe "for valid items" $ do
      prop "marks the item as done" $
        \day -> forAll todoFileIndexes $
        \(file, index) -> getItem (fst $ doItems day file [index]) index `shouldBe`
                          fmap (flip markAsDone day) (getItem file index)
      prop "reports that it marks item as done" $
        \day -> forAll todoFileIndexes $
        \(file, index) ->
        let (newTodoFile, events) = doItems day file [index] in
        events `shouldBe` [TaskChanged Done index (unsafeGetItem file index)
                           (unsafeGetItem newTodoFile index)]

    describe "no items given" $ do
      prop "leaves file unchanged and performs no actions" $
        \file day -> doItems day file [] `shouldBe` (file, [])


spec :: Spec
spec = describe "Core operations on todos" $ do
  actionsSpec
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
        forAll (choose ('a', 'z')) $ \x -> makePriority x `shouldBe` makePriority (toUpper x)

      prop "creates the same as unsafeMakePriority" $
        forAll arbitraryPriorityLetter $ \x -> makePriority x `shouldBe` Just (unsafeMakePriority x)

      prop "returns Nothing for non-priorities" $
        \x -> not (isAlpha x) ==> makePriority x `shouldBe` Nothing

  describe "marking items as done" $ do
    prop "markAsDone implies isDone" $
      \date item -> isDone (markAsDone date item)
    prop "markAsDone sets date completed if not done" $
      \date item -> not (isDone item) ==> dateCompleted (markAsDone item date) `shouldBe` Just date
    prop "markAsDone leaves date completed if already done" $
      \date item -> isDone item ==> dateCompleted (markAsDone item date) `shouldBe` (dateCompleted item)
    prop "markAsUndone implies not done" $
      not . isDone . markAsUndone

  describe "setting priorities" $ do
    prop "setting a priority shoes there *is* a priority" $
      forAll arbitraryPriority $ \pri ->
      \item -> hasPriority (prioritize item pri)
    prop "setting no priority shows there is not a priority" $
      \item -> not . hasPriority $ prioritize item noPriority

  describe "todo files" $ do
    describe "numItems" $ do
      prop "shows zero when there are no items" $
        \x -> numItems (makeTodoFile x []) `shouldBe` 0
      prop "shows the number of items" $
        \name items -> numItems (makeTodoFile name items) `shouldBe` length items
    describe "isEmpty" $ do
      prop "is equivalent to numItems is 0" $
        \x -> (numItems x == 0) `shouldBe` isEmpty x
    describe "listItems" $ do
      prop "lists items with 1-based index" $
        \name items -> listItems (makeTodoFile name items) `shouldBe` zip [1..] items
      prop "is consistent with numItems" $
        \file -> length (listItems file) `shouldBe` numItems (file)
    describe "filterItems" $ do
      prop "includes only items that satisfy a predicate" $
        \name items -> all (isDone . snd) $ filterItems isDone (makeTodoFile name items)
      prop "refers to items by their index in the original list" $
        \name items -> all (\(i, t) -> items !! (i - 1) == t) $ filterItems isDone (makeTodoFile name items)
      prop "is equivalent to listItems" $ do
        \file -> filterItems (const True) file `shouldBe` listItems file
    describe "allItems" $ do
      prop "is equivalent to listItems but without the indexes" $
        \file -> allItems file `shouldBe` map snd (listItems file)
    describe "getItem" $ do
      prop "returns Nothing when out of range" $
        \name items i -> i < 1 || i > length items ==> getItem (makeTodoFile name items) i `shouldBe` Nothing
      prop "returns the item when in range (1-based index)" $
        \name -> forAll (listOf1 arbitrary) $ \items ->
        forAll (choose (1, length items)) $ \i ->
        getItem (makeTodoFile name items) i `shouldBe` Just (items !! (i - 1))
      prop "aligns with listItem" $
        forAll todoFileIndexes $ \(file, i) ->
        [(i, fromJust (getItem file i))] `shouldBe` [(n, t) | (n, t) <- listItems file, i == n]
    describe "unsafeGetItem" $ do
      prop "returns the item when in range (1-based index)" $
        forAll todoFileIndexes $ \(file, i) ->
        getItem file i `shouldBe` Just (unsafeGetItem file i)
    describe "replaceItem" $ do
      prop "swaps out the specified item" $
        \item -> forAll todoFileIndexes $ \(file, i) ->
        let oldList = listItems file
            newList = listItems (replaceItem file i item) in
        newList `shouldBe` take (i - 1) oldList ++ [(i, item)] ++ drop i oldList
      prop "does not alter the size of the todo file" $
        \item -> forAll todoFileIndexes $ \(file, i) ->
        numItems (replaceItem file i item) `shouldBe` numItems file

  describe "archive" $ do
    prop "strips all done items" $
      all (not . isDone) . allItems . fst . archive
    prop "does not remove any not done items" $
      \file -> (allItems . fst . archive) file `shouldBe` filter (not . isDone) (allItems file)
    prop "returns only done items" $
      all isDone . snd . archive
    prop "returns precisely the done items" $
      \file -> snd (archive file) `shouldBe` filter isDone (allItems file)
    prop "every item in return value" $
      \file -> let (newFile, doneItems) = archive file in
      allItems newFile ++ doneItems `shouldMatchList` allItems file
