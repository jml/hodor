{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hodor.Types where

import Prelude hiding (concatMap)

import Data.Char (isAsciiUpper, isAsciiLower, toUpper)
import Data.Foldable (concatMap, toList)
import Data.List (nub, sort)
import Data.Maybe (isJust, mapMaybe)
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


toProject :: String -> Maybe Project
toProject ('+':[])      = Nothing
toProject ('+':project) = Just (Project project)
toProject _             = Nothing


data Context = Context String deriving (Eq, Ord, Show)


toContext :: String -> Maybe Context
toContext ('@':[])      = Nothing
toContext ('@':context) = Just (Context context)
toContext _             = Nothing


data TodoItem = TodoItem {
  dateCompleted :: Maybe Day,
  priority :: Priority,
  dateCreated :: Maybe Day,
  description :: String
} deriving (Eq, Ord)

instance Show TodoItem where
  show = show . unparse


-- XXX: The 'Ord' haskell picks isn't the one todo uses. Not sure if that
-- matters.

-- XXX: Possibly make a type for numbered todo items? Gets used in a lot of
-- places. See XXX: NumberedTodoItem

-- TODO: UNTESTED: projects
projects :: TodoItem -> [Project]
projects = mapMaybe toProject . words . description

-- TODO: UNTESTED: contexts
contexts :: TodoItem -> [Context]
contexts = mapMaybe toContext . words . description


startsWith :: Eq a => a -> [a] -> Bool
startsWith _ []    = False
startsWith c (x:_) = c == x


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


amendDescription :: TodoItem -> String -> TodoItem
amendDescription item d = item { description = d }


appendDescription :: TodoItem -> String -> TodoItem
appendDescription item suffix = amendDescription item (description item ++ ' ':suffix)


prependDescription :: TodoItem -> String -> TodoItem
prependDescription item prefix = amendDescription item (prefix ++ ' ':description item)


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


allItems :: TodoFile -> [TodoItem]
allItems = toList . todoFileItemsV

-- XXX: NumberedTodoItem
listItems :: TodoFile -> [(Int, TodoItem)]
listItems = zip [1..] . allItems


-- XXX: NumberedTodoItem
filterItems :: (TodoItem -> Bool) -> TodoFile -> [(Int, TodoItem)]
filterItems p = filter (p . snd) . listItems


numItems :: TodoFile -> Int
numItems = S.length . todoFileItemsV


isEmpty :: TodoFile -> Bool
isEmpty = (==) 0 . numItems


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


archive :: TodoFile -> (TodoFile, [TodoItem])
archive file =
  let items = todoFileItemsV file
      (doneItems, todoItems) = S.partition isDone items
      newTodoFile = file { todoFileItemsV = todoItems }
  in (newTodoFile, toList doneItems)



{- Turn todos back into strings. -}

-- XXX: Would like to move this to a separate module, but since 'description'
-- depends on unparse I don't know how to do that sanely.

-- XXX: This doesn't need to be a typeclass really. It just spares us thinking up names.

class Unparse a where
  unparse :: a -> String


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
