module Hodor.Types where

import Data.Time (Day)

type Priority = Char

data Project = Project String deriving (Eq, Ord)

instance Show Project where
  show (Project p) = '+':p

data Context = Context String deriving (Eq, Ord)

instance Show Context where
  show (Context c) = '@':c


data TodoItem = TodoItem {
  dateCompleted :: Maybe Day,
  priority :: Maybe Priority,
  dateCreated :: Maybe Day,
  projects :: [Project],
  contexts :: [Context],
  description :: String
} deriving (Show, Eq, Ord)


defaultTodoItem :: TodoItem
defaultTodoItem = TodoItem { dateCompleted = Nothing,
                             priority = Nothing,
                             dateCreated = Nothing,
                             projects = [],
                             contexts = [],
                             description = "" }


data TodoFile = TodoFile {
  todoFileName :: String,
  todoFileItems :: [TodoItem]
} deriving (Show, Eq, Ord)

