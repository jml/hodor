{-# LANGUAGE TypeSynonymInstances #-}

module Hodor.Types where

import Data.Maybe (isJust)
import Data.Time (Day, showGregorian)

type Priority = Char

data Project = Project String deriving (Eq, Ord)

instance Show Project where
  show (Project p) = '+':p

data Context = Context String deriving (Eq, Ord)

instance Show Context where
  show (Context c) = '@':c


data Token = Bareword String | ProjectToken String | ContextToken String deriving (Eq, Ord, Show)

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

defaultTodoItem :: TodoItem
defaultTodoItem = TodoItem { dateCompleted = Nothing,
                             priority = Nothing,
                             dateCreated = Nothing,
                             tokens = [] }

projects :: TodoItem -> [Project]
projects item = [ Project p | ProjectToken p <- (tokens item) ]

contexts :: TodoItem -> [Context]
contexts item = [ Context p | ContextToken p <- (tokens item) ]

description :: TodoItem -> String
description item = concatMap unparse (tokens item)

isDone :: TodoItem -> Bool
isDone = isJust . dateCompleted

markAsDone :: TodoItem -> Day -> TodoItem
markAsDone item day = item { dateCompleted = Just day }

-- XXX: How do I do post-conditions in Haskell?

instance Unparse TodoItem where
  unparse item = concat $
    case (dateCompleted item) of
      Nothing -> [unparse (priority item), unparse (dateCreated item), (description item)]
      Just completed -> ["x ", unparse completed, unparse (dateCreated item), (description item)]


-- XXX: Could make this a NamedList type class or something, implement
-- functor, foldable & traversable, and then make a specific instance
-- (newtype?) for todo.

data TodoFile = TodoFile {
  todoFileName :: String,
  todoFileItems :: [TodoItem]
} deriving (Show, Eq, Ord)


instance Unparse TodoFile where
  unparse = unlines . map unparse . todoFileItems


mapTodos :: (TodoItem -> TodoItem) -> TodoFile -> TodoFile
mapTodos f = onTodos (map f)

onTodos :: ([TodoItem] -> [TodoItem]) -> TodoFile -> TodoFile
onTodos f todoFile = todoFile { todoFileItems = f (todoFileItems todoFile) }
