{-# LANGUAGE TypeSynonymInstances #-}

module Hodor.Types where

import Control.Monad.Writer
import Data.Foldable (toList)
import Data.Maybe (isJust)
import Data.Time (Day, showGregorian)
import qualified Data.Sequence as S

-- XXX: Consider making this newtype and incorporating the Maybe so we can
-- have a better sort implementation.
type Priority = Char

data Project = Project String deriving (Eq, Ord)

instance Show Project where
  show (Project p) = '+':p

data Context = Context String deriving (Eq, Ord)

instance Show Context where
  show (Context c) = '@':c


data Token = Bareword String | ProjectToken String | ContextToken String deriving (Eq, Ord, Show)

-- XXX: Move all the 'unparse' logic together, perhaps to a separate module.
-- TODO: UNTESTED: all unparse logic
class Unparse a where
  unparse :: a -> String


instance Unparse Token where
  unparse (Bareword string) = string
  unparse (ProjectToken string) = '+':string
  unparse (ContextToken string) = '@':string


instance (Unparse a) => Unparse (Maybe a) where
  unparse Nothing = ""
  unparse (Just x) = unparse x


instance Unparse Day where
  unparse x = showGregorian x ++ " "


instance Unparse Priority where
  unparse p = ['(', p, ')', ' ']


data TodoItem = TodoItem {
  dateCompleted :: Maybe Day,
  priority :: Maybe Priority,
  dateCreated :: Maybe Day,
  tokens :: [Token]
} deriving (Show, Eq, Ord)

-- XXX: The 'Ord' haskell picks isn't the one todo uses. Not sure if that
-- matters.

-- TODO: UNTESTED: projects
projects :: TodoItem -> [Project]
projects item = [ Project p | ProjectToken p <- (tokens item) ]

-- TODO: UNTESTED: contexts
contexts :: TodoItem -> [Context]
contexts item = [ Context p | ContextToken p <- (tokens item) ]

-- TODO: UNTESTED: description
description :: TodoItem -> String
description item = concatMap unparse (tokens item)

-- TODO: UNTESTED: isDone
isDone :: TodoItem -> Bool
isDone = isJust . dateCompleted

-- XXX: How do I do post-conditions in Haskell?
-- TODO: UNTESTED: markAsDone
markAsDone :: TodoItem -> Day -> TodoItem
markAsDone item day | isDone item = item
                    | otherwise   = item { dateCompleted = Just day }


-- XXX: Possibly make this a more generic 'event' data type
data DoneResult = Done Int TodoItem |
                  AlreadyDone Int TodoItem |
                  NoSuchTask Int
                  deriving (Show, Eq)


-- XXX: Move doItem with all the TodoFile functions
doItem :: TodoFile -> Day -> Int -> Writer (S.Seq DoneResult) TodoFile
doItem file day i =
  case getItem file i of
    Nothing -> tell (S.singleton (NoSuchTask i)) >> return file
    Just todo ->
      if isDone todo
      then tell (S.singleton (AlreadyDone i todo)) >> return file
      else
        let newTodo = markAsDone todo day in
        tell (S.singleton (Done i newTodo)) >> return (replace file (i - 1) newTodo)
  where -- O(log(min(i,n-i))), i = ndx, n = length todoFileItems
        replace todoFile ndx item =
          todoFile { todoFileItemsV = S.update ndx item (todoFileItemsV todoFile) }


-- TODO: UNTESTED: getItem
-- XXX: Move getItem with all the TodoFile functions
getItem :: TodoFile -> Int -> Maybe TodoItem
getItem file i =
  if 1 <= i && i <= numItems file
  then Just (todoFileItemsV file `S.index` (i - 1))
  else Nothing


-- TODO: UNTESTED: unsafeGetItem
-- XXX: Move unsafeGetItem with all the TodoFile functions
unsafeGetItem :: TodoFile -> Int -> TodoItem
unsafeGetItem file i =
  case getItem file i of
    Just item -> item
    Nothing -> error $ "No such item: " ++ (show i)


-- XXX: There's a way to apply a function to the second element of a tuple
-- using arrows. Use that.
-- XXX: Move 'doItems' together with all the TodoFile functions
doItems :: TodoFile -> Day -> [Int] -> (TodoFile, [DoneResult])
doItems file day indexes =
  let (todo, results) = runWriter $ foldM (flip doItem day) file indexes in
  (todo, toList results)


-- TODO: UNTESTED: archive
-- XXX: Move 'archive' together with all the TodoFile functions
archive :: TodoFile -> (TodoFile, [TodoItem])
archive file =
  let items = todoFileItemsV file
      (doneItems, todoItems) = S.partition isDone items
      newTodoFile = file { todoFileItemsV = todoItems }
  in (newTodoFile, toList doneItems)


-- XXX: Move the Unparse TodoItem declaration together with the rest
instance Unparse TodoItem where
  unparse item = concat $
    case (dateCompleted item) of
      Nothing -> [unparse (priority item), unparse (dateCreated item), (description item)]
      Just completed -> ["x ", unparse completed, unparse (dateCreated item), (description item)]


-- XXX: Could make this a NamedList type class or something, implement
-- functor, foldable & traversable, and then make a specific instance
-- (newtype?) for todo.

-- XXX: Come up with (or research) better naming convention for internal
-- details, and apply this to record accessors.

data TodoFile = TodoFile {
  todoFileName :: String,
  todoFileItemsV :: S.Seq TodoItem
} deriving (Show, Eq, Ord)


instance Unparse TodoFile where
  unparse = unlines . toList . fmap unparse . todoFileItemsV


-- TODO: UNTESTED: makeTodoFile
makeTodoFile :: String -> [TodoItem] -> TodoFile
makeTodoFile name items = TodoFile { todoFileName = name,
                                     todoFileItemsV = S.fromList items }


-- TODO: UNTESTED: updateTodoFile
updateTodoFile :: TodoFile -> [TodoItem] -> TodoFile
updateTodoFile old = makeTodoFile (todoFileName old)


-- XXX: Stop exporting this: it's just a compatibility method.
todoFileItems :: TodoFile -> [TodoItem]
todoFileItems = toList . todoFileItemsV


listItems :: TodoFile -> [(Int, TodoItem)]
listItems = zip [1..] . toList . todoFileItemsV


-- TODO: UNTESTED: numItems
numItems :: TodoFile -> Int
numItems = S.length . todoFileItemsV
