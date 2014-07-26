{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hodor.Types where

import Prelude hiding (concatMap)

import Control.Arrow (second)
import Control.Monad.Writer
import Data.Char (isAsciiUpper, isAsciiLower, toUpper)
import Data.Foldable (concatMap, toList)
import Data.List (nub, sort)
import Data.Maybe (isJust)
import Data.Time (Day, showGregorian)
import qualified Data.Sequence as S


data Priority = Pri Char | NoPri deriving (Show, Eq)


instance Ord Priority where
  NoPri <= x = (x == NoPri)
  _ <= NoPri = True
  (Pri x) <= (Pri y) = x < y


makePriority :: Char -> Maybe Priority
makePriority c | isAsciiUpper c = Just . Pri $ c
               | isAsciiLower c = Just . Pri . toUpper $ c
               | otherwise      = Nothing

unsafeMakePriority :: Char -> Priority
unsafeMakePriority c =
  case makePriority c of
    Just p -> p
    Nothing -> error ("Invalid priority " ++ show c ++ ", must be upper-case character")

noPriority :: Priority
noPriority = NoPri

isPriority :: Priority -> Bool
isPriority (Pri _) = True
isPriority _       = False


data Project = Project String deriving (Eq, Ord, Show)

data Context = Context String deriving (Eq, Ord, Show)

data Token = Bareword String | ProjectToken String | ContextToken String deriving (Eq, Ord, Show)


data TodoItem = TodoItem {
  dateCompleted :: Maybe Day,
  priority :: Priority,
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

isDone :: TodoItem -> Bool
isDone = isJust . dateCompleted

hasPriority :: TodoItem -> Bool
hasPriority = isPriority . priority

markAsDone :: TodoItem -> Day -> TodoItem
markAsDone item day | isDone item = item
                    | otherwise   = item { dateCompleted = Just day }


markAsUndone :: TodoItem -> TodoItem
markAsUndone item = item { dateCompleted = Nothing }


prioritize :: TodoItem -> Priority -> TodoItem
prioritize item p = item { priority = p }


-- XXX: Could make this a NamedList type class or something, implement
-- functor, foldable & traversable, and then make a specific instance
-- (newtype?) for todo.

-- XXX: Come up with (or research) better naming convention for internal
-- details, and apply this to record accessors.

data TodoFile = TodoFile {
  todoFileName :: String,
  todoFileItemsV :: S.Seq TodoItem
} deriving (Show, Eq, Ord)


makeTodoFile :: String -> [TodoItem] -> TodoFile
makeTodoFile name items = TodoFile { todoFileName = name,
                                     todoFileItemsV = S.fromList items }


-- XXX: NumberedTodoItem
listItems :: TodoFile -> [(Int, TodoItem)]
listItems = zip [1..] . toList . todoFileItemsV


-- XXX: NumberedTodoItem
filterItems :: (TodoItem -> Bool) -> TodoFile -> [(Int, TodoItem)]
filterItems p = filter (p . snd) . listItems


numItems :: TodoFile -> Int
numItems = S.length . todoFileItemsV


getItem :: TodoFile -> Int -> Maybe TodoItem
getItem file i =
  if 1 <= i && i <= numItems file
  then Just (todoFileItemsV file `S.index` (i - 1))
  else Nothing


unsafeGetItem :: TodoFile -> Int -> TodoItem
unsafeGetItem file i =
  case getItem file i of
    Just item -> item
    Nothing -> error $ "No such item: " ++ (show i)


-- O(log(min(i,n-i))), i = ndx, n = length todoFileItems
replaceItem :: TodoFile -> Int -> TodoItem -> TodoFile
replaceItem file i item =
  file { todoFileItemsV = S.update (i - 1) item (todoFileItemsV file) }


allContexts :: TodoFile -> [Context]
allContexts = nub . sort . concatMap contexts . todoFileItemsV


allProjects :: TodoFile -> [Project]
allProjects = nub . sort . concatMap projects . todoFileItemsV


-- TODO: UNTESTED: archive
archive :: TodoFile -> (TodoFile, [TodoItem])
archive file =
  let items = todoFileItemsV file
      (doneItems, todoItems) = S.partition isDone items
      newTodoFile = file { todoFileItemsV = todoItems }
  in (newTodoFile, toList doneItems)


-- XXX: NumberedTodoItem
data TodoEvent =
  NoSuchTask Int |
  TaskChanged TaskAction Int TodoItem TodoItem
  deriving (Show, Eq)


data TaskAction =
  Done |
  Undone |
  Prioritized |
  Deprioritized
  deriving (Show, Eq)


newtype TodoEvents a = TodoEvents { getWriter :: Writer (S.Seq TodoEvent) a }
                       deriving (Monad)


_runEvents :: TodoEvents a -> (a, [TodoEvent])
_runEvents = second toList . runWriter . getWriter


_logEvent :: TodoEvent -> TodoEvents ()
_logEvent e = TodoEvents (tell (S.singleton e))


_getItem :: TodoFile -> Int -> TodoEvents (Maybe TodoItem)
_getItem file i = do
  case getItem file i of
    Nothing -> do
      _logEvent $ NoSuchTask i
      return Nothing
    Just x -> return (Just x)


_adjustItem :: TaskAction -> (TodoItem -> TodoItem) -> TodoFile -> Int -> TodoEvents TodoFile
_adjustItem action f file i = do
  item <- _getItem file i
  case item of
    Nothing -> return file
    Just todo -> do
      newTodo <- return $ f todo
      _logEvent (TaskChanged action i todo newTodo)
      return (replaceItem file i newTodo)


_adjustItems :: TaskAction -> (TodoItem -> TodoItem) -> TodoFile -> [Int] -> TodoEvents TodoFile
_adjustItems action = foldM . (_adjustItem action)


doItems :: Day -> TodoFile -> [Int] -> (TodoFile, [TodoEvent])
doItems day file = _runEvents . _adjustItems Done (flip markAsDone day) file


undoItems :: TodoFile -> [Int] -> (TodoFile, [TodoEvent])
undoItems file = _runEvents . _adjustItems Undone markAsUndone file


prioritizeItem :: Priority -> TodoFile -> Int -> (TodoFile, [TodoEvent])
prioritizeItem p file = _runEvents . _adjustItem Prioritized (flip prioritize p) file


deprioritizeItem :: TodoFile -> Int -> (TodoFile, [TodoEvent])
deprioritizeItem file = _runEvents . _adjustItem Deprioritized (flip prioritize noPriority) file


{- Turn todos back into strings. -}

-- XXX: Would like to move this to a separate module, but since 'description'
-- depends on unparse I don't know how to do that sanely.

-- XXX: This doesn't need to be a typeclass really. It just spares us thinking up names.

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
  unparse NoPri  = []


instance Unparse TodoItem where
  unparse item = concat $
    case (dateCompleted item) of
      Nothing -> [unparse (priority item), unparse (dateCreated item), (description item)]
      Just completed -> ["x ", unparse completed, unparse (dateCreated item), (description item)]


instance Unparse TodoFile where
  unparse = unlines . toList . fmap unparse . todoFileItemsV
