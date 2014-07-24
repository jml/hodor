{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hodor.Types where

import Prelude hiding (concatMap)

import Control.Arrow (second)
import Control.Monad.Writer
import Data.Foldable (concatMap, toList)
import Data.List (nub, sort)
import Data.Maybe (isJust)
import Data.Time (Day, showGregorian)
import qualified Data.Sequence as S


-- XXX: Consider making this newtype and incorporating the Maybe so we can
-- have a better sort implementation.
newtype Priority = Pri Char deriving (Show, Eq, Ord)

makePriority :: Char -> Priority
makePriority = Pri


data Project = Project String deriving (Eq, Ord)

instance Show Project where
  show (Project p) = '+':p


data Context = Context String deriving (Eq, Ord)

instance Show Context where
  show (Context c) = '@':c


data Token = Bareword String | ProjectToken String | ContextToken String deriving (Eq, Ord, Show)

data TodoItem = TodoItem {
  dateCompleted :: Maybe Day,
  priority :: Maybe Priority,
  dateCreated :: Maybe Day,
  tokens :: [Token]
} deriving (Show, Eq, Ord)

-- XXX: The 'Ord' haskell picks isn't the one todo uses. Not sure if that
-- matters.

-- XXX: Possibly make a type for numbered todo items? Gets used in a lot of
-- places. See XXX: NumberedTodoItem

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

-- TODO: UNTESTED: hasPriority
hasPriority :: TodoItem -> Bool
hasPriority = isJust . priority

-- XXX: How do I do post-conditions in Haskell?
-- TODO: UNTESTED: markAsDone
markAsDone :: TodoItem -> Day -> TodoItem
markAsDone item day | isDone item = item
                    | otherwise   = item { dateCompleted = Just day }


markAsUndone :: TodoItem -> TodoItem
markAsUndone item = item { dateCompleted = Nothing }


-- XXX: NumberedTodoItem
data TodoEvent = Done Int TodoItem |
                  AlreadyDone Int TodoItem |
                  NoSuchTask Int |
                  Undone Int TodoItem |
                  AlreadyNotDone Int TodoItem
                  deriving (Show, Eq)


-- XXX: Could make this a NamedList type class or something, implement
-- functor, foldable & traversable, and then make a specific instance
-- (newtype?) for todo.

-- XXX: Come up with (or research) better naming convention for internal
-- details, and apply this to record accessors.

data TodoFile = TodoFile {
  todoFileName :: String,
  todoFileItemsV :: S.Seq TodoItem
} deriving (Show, Eq, Ord)


-- TODO: UNTESTED: makeTodoFile
makeTodoFile :: String -> [TodoItem] -> TodoFile
makeTodoFile name items = TodoFile { todoFileName = name,
                                     todoFileItemsV = S.fromList items }


-- TODO: UNTESTED: updateTodoFile
updateTodoFile :: TodoFile -> [TodoItem] -> TodoFile
updateTodoFile old = makeTodoFile (todoFileName old)


-- XXX: NumberedTodoItem
listItems :: TodoFile -> [(Int, TodoItem)]
listItems = zip [1..] . toList . todoFileItemsV


allItems :: TodoFile -> S.Seq TodoItem
allItems = todoFileItemsV


allContexts :: TodoFile -> [Context]
allContexts = nub . sort . concatMap contexts . todoFileItemsV


allProjects :: TodoFile -> [Project]
allProjects = nub . sort . concatMap projects . todoFileItemsV


-- TODO: UNTESTED: numItems
numItems :: TodoFile -> Int
numItems = S.length . todoFileItemsV


-- XXX: NumberedTodoItem
-- TODO: UNTESTED: NumberedTodoItem
filterItems :: (TodoItem -> Bool) -> TodoFile -> [(Int, TodoItem)]
filterItems p = filter (p . snd) . listItems


-- TODO: UNTESTED: getItem
getItem :: TodoFile -> Int -> Maybe TodoItem
getItem file i =
  if 1 <= i && i <= numItems file
  then Just (todoFileItemsV file `S.index` (i - 1))
  else Nothing


-- TODO: UNTESTED: unsafeGetItem
unsafeGetItem :: TodoFile -> Int -> TodoItem
unsafeGetItem file i =
  case getItem file i of
    Just item -> item
    Nothing -> error $ "No such item: " ++ (show i)


-- TODO: UNTESTED: archive
archive :: TodoFile -> (TodoFile, [TodoItem])
archive file =
  let items = todoFileItemsV file
      (doneItems, todoItems) = S.partition isDone items
      newTodoFile = file { todoFileItemsV = todoItems }
  in (newTodoFile, toList doneItems)


newtype TodoEvents a = TodoEvents { getWriter :: Writer (S.Seq TodoEvent) a }
                       deriving (Monad)


runEvents :: TodoEvents a -> (a, [TodoEvent])
runEvents = second toList . runWriter . getWriter


logEvent :: TodoEvent -> TodoEvents ()
logEvent e = TodoEvents (tell (S.singleton e))


_getItem :: TodoFile -> Int -> TodoEvents (Maybe TodoItem)
_getItem file i = do
  case getItem file i of
    Nothing -> do
      logEvent $ NoSuchTask i
      return Nothing
    Just x -> return (Just x)


doItem :: TodoFile -> Day -> Int -> TodoEvents TodoFile
doItem file day i = do
  item <- _getItem file i
  case item of
    Nothing -> return file
    Just todo ->
      if isDone todo
      then do
        logEvent $ AlreadyDone i todo
        return file
      else do
        let newTodo = markAsDone todo day
        logEvent $ Done i newTodo
        return (replaceItem file i newTodo)


doItems :: TodoFile -> Day -> [Int] -> (TodoFile, [TodoEvent])
doItems file day = runEvents . foldM (flip doItem day) file


undoItem :: TodoFile -> Int -> TodoEvents TodoFile
undoItem file i = do
  item <- _getItem file i
  case item of
    Nothing -> return file
    Just todo ->
      if not (isDone todo)
      then do
        logEvent $ AlreadyNotDone i todo
        return file
      else do
        let newTodo = markAsUndone todo
        logEvent $ Undone i newTodo
        return (replaceItem file i newTodo)


undoItems :: TodoFile -> [Int] -> (TodoFile, [TodoEvent])
undoItems file = runEvents . foldM undoItem file


-- O(log(min(i,n-i))), i = ndx, n = length todoFileItems
replaceItem :: TodoFile -> Int -> TodoItem -> TodoFile
replaceItem file i item =
  file { todoFileItemsV = S.update (i - 1) item (todoFileItemsV file) }

{- Turn todos back into strings. -}

-- TODO: UNTESTED: all unparse logic

-- XXX: Would like to move this to a separate module, but since 'description'
-- depends on unparse I don't know how to do that sanely.

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
  unparse (Pri p) = ['(', p, ')', ' ']


instance Unparse TodoItem where
  unparse item = concat $
    case (dateCompleted item) of
      Nothing -> [unparse (priority item), unparse (dateCreated item), (description item)]
      Just completed -> ["x ", unparse completed, unparse (dateCreated item), (description item)]


instance Unparse TodoFile where
  unparse = unlines . toList . fmap unparse . todoFileItemsV
