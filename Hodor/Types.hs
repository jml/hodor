module Hodor.Types where

import Data.Time (Day)

type Priority = Char

data Project = Project String deriving (Eq, Ord)

instance Show Project where
  show (Project p) = '+':p

data Context = Context String deriving (Eq, Ord)

instance Show Context where
  show (Context c) = '@':c


data Token = Bareword String | ProjectToken String | ContextToken String deriving (Eq, Ord, Show)


formatToken :: Token -> String
formatToken (Bareword string) = string
formatToken (ProjectToken string) = '+':string
formatToken (ContextToken string) = '@':string


data TodoItem = TodoItem {
  dateCompleted :: Maybe Day,
  priority :: Maybe Priority,
  dateCreated :: Maybe Day,
  tokens :: [Token]
} deriving (Show, Eq, Ord)


defaultTodoItem :: TodoItem
defaultTodoItem = TodoItem { dateCompleted = Nothing,
                             priority = Nothing,
                             dateCreated = Nothing,
                             tokens = [] }

-- XXX: Restore description
-- XXX: Restore projects
-- XXX: Restore contexts

projects :: TodoItem -> [Project]
projects item = [ Project p | ProjectToken p <- (tokens item) ]

contexts :: TodoItem -> [Context]
contexts item = [ Context p | ContextToken p <- (tokens item) ]

description :: TodoItem -> String
description item = concatMap formatToken (tokens item)


data TodoFile = TodoFile {
  todoFileName :: String,
  todoFileItems :: [TodoItem]
} deriving (Show, Eq, Ord)

