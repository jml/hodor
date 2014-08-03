module Tests.TypesSpec (spec) where

import Data.Char (isAlpha, toUpper)
import Data.List ((\\), nub)
import Data.Maybe (fromJust)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import Hodor.Actions (
  appendItem,
  deprioritizeItem,
  doItems,
  prependItem,
  prioritizeItem,
  TaskAction(..),
  TodoEvent(..),
  undoItems,
  )
import Hodor.Types (
  allItems,
  amendDescription,
  appendDescription,
  archive,
  dateCompleted,
  description,
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
  prependDescription,
  prioritize,
  replaceItem,
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


todoFileIndex :: Gen (TodoFile, Int)
todoFileIndex = do
  file <- nonEmptyTodoFile
  index <- indexesOf file
  return (file, index)


todoFileIndexes :: Gen (TodoFile, [Int])
todoFileIndexes = do
  file <- nonEmptyTodoFile
  index <- listOf1 $ indexesOf file
  return (file, index)


type Transformation a = (TodoFile -> a -> (TodoFile, [TodoEvent]))


prop_emptyUnchanged_multiple :: Transformation [Int] -> Property
prop_emptyUnchanged_multiple transformation =
  property $ \items -> transformation emptyFile items `shouldBe` (emptyFile, map NoSuchTask (nub items))

prop_emptyUnchanged_single :: Transformation Int -> Property
prop_emptyUnchanged_single transformation =
  property $ \item -> transformation emptyFile item `shouldBe` (emptyFile, [NoSuchTask item])

prop_invalidUnchanged_multiple :: Transformation [Int] -> Property
prop_invalidUnchanged_multiple transformation =
  property $ \file -> forAll (listOf $ invalidIndexesOf file) $ \items ->
  transformation file items `shouldBe` (file, map NoSuchTask (nub items))

prop_invalidUnchanged_single :: Transformation Int -> Property
prop_invalidUnchanged_single transformation =
  property $ \file -> forAll (invalidIndexesOf file) $ \item ->
  transformation file item `shouldBe` (file, [NoSuchTask item])

prop_reportsEvent_multiple :: TaskAction -> Transformation [Int] -> Property
prop_reportsEvent_multiple action transformation =
  forAll todoFileIndexes $
  \(file, indexes) ->
  let (newTodoFile, events) = transformation file indexes in
  events `shouldBe` (map (\i -> TaskChanged action i (unsafeGetItem file i)
                                (unsafeGetItem newTodoFile i)) (nub indexes))

prop_reportsEvent_single :: TaskAction -> Transformation Int -> Property
prop_reportsEvent_single action transformation =
  forAll todoFileIndex $
  \(file, index) ->
  let (newTodoFile, events) = transformation file index in
  events `shouldBe` [TaskChanged action index (unsafeGetItem file index)
                     (unsafeGetItem newTodoFile index)]

prop_ignoresUnmentioned_multiple :: Transformation [Int] -> Property
prop_ignoresUnmentioned_multiple transformation =
  property $ \oldFile items ->
  let (newFile, _) = transformation oldFile items
      nonItems = [1..numItems oldFile] \\ items in
  map (getItem newFile) nonItems `shouldBe` map (getItem oldFile) nonItems

prop_ignoresUnmentioned_single :: Transformation Int -> Property
prop_ignoresUnmentioned_single transformation =
  property $ \oldFile item ->
  let (newFile, _) = transformation oldFile item
      nonItems = [1..numItems oldFile] \\ [item] in
  map (getItem newFile) nonItems `shouldBe` map (getItem oldFile) nonItems


prop_uniqueEvent_multiple :: Transformation [Int] -> Property
prop_uniqueEvent_multiple transformation =
  property $
  \file items -> (length $ snd $ transformation file items) `shouldBe` length (nub items)


prop_uniqueEvent_single :: Transformation Int -> Property
prop_uniqueEvent_single transformation =
  property $ \file item -> (length $ snd $ transformation file item) `shouldBe` 1


prop_noItemsGiven :: Transformation [a] -> Property
prop_noItemsGiven transformation =
  property $ \file -> transformation file [] `shouldBe` (file, [])


itemsTransformations :: TaskAction -> Transformation [Int] -> Property
itemsTransformations action transformation = conjoin [
  prop_emptyUnchanged_multiple transformation,
  prop_invalidUnchanged_multiple transformation,
  prop_reportsEvent_multiple action transformation,
  prop_ignoresUnmentioned_multiple transformation,
  prop_uniqueEvent_multiple transformation,
  prop_noItemsGiven transformation
  ]


itemTransformation :: TaskAction -> Transformation Int -> Property
itemTransformation action transformation = conjoin [
  prop_emptyUnchanged_single transformation,
  prop_invalidUnchanged_single transformation,
  prop_reportsEvent_single action transformation,
  prop_ignoresUnmentioned_single transformation,
  prop_uniqueEvent_single transformation
  ]


prop_doesNotRemoveTodos :: (TodoFile -> a -> (TodoFile, b)) -> (TodoFile -> a -> Expectation)
prop_doesNotRemoveTodos transformation =
  \file item -> (numItems $ fst $ transformation file item) `shouldBe` numItems file


actionsSpec :: Spec
actionsSpec = describe "High-level operations on todos" $ do
  describe "mark as done" $ do

    prop "common properties" $
      \day -> itemsTransformations Done (doItems day)

    prop "marked all the valid items as done" $
      \file day items -> let (newFile, _) = doItems day file items in
      map (getItem newFile) items `shouldBe` map (fmap (flip markAsDone day) . getItem file) items

    prop "does not remove items from todo" $ \day -> prop_doesNotRemoveTodos (doItems day)


  describe "mark as undone" $ do

    prop "common properties" $
      itemsTransformations Undone undoItems

    prop "marked all the valid items as not done" $
      \file items -> let (newFile, _) = undoItems file items in
      map (getItem newFile) items `shouldBe` map (fmap markAsUndone . getItem file) items

    prop "does not remove items from todo" $ prop_doesNotRemoveTodos undoItems


  describe "prioritize" $ do

    prop "common single-item properties" $
      \pri -> itemTransformation Prioritized (prioritizeItem pri)

    describe "for valid items" $ do
      prop "marks the item as prioritized" $
        \pri ->
        forAll todoFileIndex $
        \(file, index) -> let (newFile, _) = prioritizeItem pri file index in
        unsafeGetItem newFile index `shouldBe` prioritize (unsafeGetItem file index) pri

      prop "does not remove items from todo" $
        \pri -> prop_doesNotRemoveTodos (prioritizeItem pri)


  describe "deprioritize" $ do

    prop "common single-item properties" $
      itemTransformation Deprioritized deprioritizeItem

    describe "for valid items" $ do
      prop "marks the item as deprioritized" $
        forAll todoFileIndex $
        \(file, index) -> let (newFile, _) = deprioritizeItem file index in
        unsafeGetItem newFile index `shouldBe`
        prioritize (unsafeGetItem file index) noPriority

    prop "does not remove items from todo" $
      prop_doesNotRemoveTodos deprioritizeItem


  describe "append" $ do

    prop "common single-item properties" $
      \string -> itemTransformation Amend (appendItem string)

    describe "for valid items" $ do
      prop "appends text to the item" $
        \string ->
        forAll todoFileIndex $
        \(file, index) -> let (newFile, _) = appendItem string file index in
        unsafeGetItem newFile index `shouldBe`
        appendDescription (unsafeGetItem file index) string

    prop "does not remove items from todo" $
      \string -> prop_doesNotRemoveTodos (appendItem string)


  describe "prepend" $ do

    prop "common single-item properties" $
      \string -> itemTransformation Amend (prependItem string)

    describe "for valid items" $ do
      prop "appends text to the item" $
        \string ->
        forAll todoFileIndex $
        \(file, index) -> let (newFile, _) = prependItem string file index in
        unsafeGetItem newFile index `shouldBe`
        prependDescription (unsafeGetItem file index) string

    prop "does not remove items from todo" $
      \string -> prop_doesNotRemoveTodos (prependItem string)


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

  describe "changing description" $ do
    prop "setting description updates the description" $
      \item newDesc -> description (amendDescription item newDesc) `shouldBe` newDesc
    prop "append adds to the description" $
      \item suffix -> description (appendDescription item suffix) `shouldBe`
                      description item ++ ' ':suffix
    prop "prepend adds to the description" $
      \item prefix -> description (prependDescription item prefix) `shouldBe`
                      prefix ++ ' ':description item

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
        forAll todoFileIndex $ \(file, i) ->
        [(i, fromJust (getItem file i))] `shouldBe` [(n, t) | (n, t) <- listItems file, i == n]
    describe "unsafeGetItem" $ do
      prop "returns the item when in range (1-based index)" $
        forAll todoFileIndex $ \(file, i) ->
        getItem file i `shouldBe` Just (unsafeGetItem file i)
    describe "replaceItem" $ do
      prop "swaps out the specified item" $
        \item -> forAll todoFileIndex $ \(file, i) ->
        let oldList = listItems file
            newList = listItems (replaceItem file i item) in
        newList `shouldBe` take (i - 1) oldList ++ [(i, item)] ++ drop i oldList
      prop "does not alter the size of the todo file" $
        \item -> forAll todoFileIndex $ \(file, i) ->
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


