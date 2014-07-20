{-# LANGUAGE TypeSynonymInstances #-}

module Hodor.Types where

import Control.Monad.Writer
import Data.Foldable (toList)
import Data.Maybe (isJust)
import Data.Time (Day, showGregorian)
import qualified Data.Sequence as S

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

-- XXX: How do I do post-conditions in Haskell?
markAsDone :: TodoItem -> Day -> TodoItem
markAsDone item day | isDone item = item
                    | otherwise   = item { dateCompleted = Just day }


data DoneResult = Done Int TodoItem |
                  AlreadyDone Int TodoItem |
                  NoSuchTask Int
                  deriving (Show, Eq)


-- O(n), n is size of TodoFile
-- XXX: Currently 1-based (that's what enumerate does). Ideally would be
-- 0-based and we'd transform.
doItem :: TodoFile -> Day -> Int -> Writer (S.Seq DoneResult) TodoFile
doItem file day index =
  if index > numItems || index < 1
  then tell (S.singleton (NoSuchTask index)) >> return file
  else
    let todo = allItems !! (index - 1) in
    if isDone todo
    then tell (S.singleton (AlreadyDone index todo)) >> return file
    else
      let newTodo = markAsDone todo day in
      tell (S.singleton (Done index newTodo)) >> return (replace file index newTodo)
  where allItems = todoFileItems file
        numItems = length allItems
        replace todoFile ndx item =
          onTodos (\items -> [ if i == ndx then item else x | (i, x) <- zip [1..] items]) todoFile


-- XXX: Currently O(N * M), worst case O(N ** 2). Interesting exercise to
-- rewrite as performant with lists, but maybe better just to replace core
-- type with Vector?

-- XXX: There's a way to apply a function to the second element of a tuple
-- using arrows. Use that.
doItems :: TodoFile -> Day -> [Int] -> (TodoFile, [DoneResult])
doItems file day indexes =
  let (todo, results) = runWriter $ foldM (flip doItem day) file indexes in
  (todo, toList results)


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
  todoFileItemsV :: S.Seq TodoItem
} deriving (Show, Eq, Ord)


instance Unparse TodoFile where
  unparse = unlines . map unparse . todoFileItems


makeTodoFile :: String -> [TodoItem] -> TodoFile
makeTodoFile name items = TodoFile { todoFileName = name,
                                     todoFileItemsV = S.fromList items }


updateTodoFile :: TodoFile -> [TodoItem] -> TodoFile
updateTodoFile old = makeTodoFile (todoFileName old)


todoFileItems :: TodoFile -> [TodoItem]
todoFileItems = toList . todoFileItemsV


mapTodos :: (TodoItem -> TodoItem) -> TodoFile -> TodoFile
mapTodos f t = t { todoFileItemsV = (fmap f . todoFileItemsV $ t) }


onTodos :: ([TodoItem] -> [TodoItem]) -> TodoFile -> TodoFile
onTodos f todoFile =
  todoFile { todoFileItemsV = (S.fromList . f . toList . todoFileItemsV) todoFile }
