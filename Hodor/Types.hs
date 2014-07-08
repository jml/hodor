{-# LANGUAGE TypeSynonymInstances #-}

module Hodor.Types where

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

instance Unparse TodoItem where
  unparse item = concat $
    case (dateCompleted item) of
      Nothing -> [unparse (priority item), unparse (dateCreated item), (description item)]
      Just completed -> ["x ", unparse completed, unparse (dateCreated item), (description item)]


data TodoFile = TodoFile {
  todoFileName :: String,
  todoFileItems :: [TodoItem]
} deriving (Show, Eq, Ord)
