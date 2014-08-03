{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hodor.Actions where

import Control.Arrow (second)
import Control.Monad.Writer
import Data.Foldable (toList)
import Data.List (nub)
import Data.Time (Day)
import qualified Data.Sequence as S
import Hodor.Types (
  getItem,
  markAsDone,
  markAsUndone,
  noPriority,
  prioritize,
  Priority,
  replaceItem,
  TodoItem,
  TodoFile,
  )

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
_adjustItems action f file items = foldM (_adjustItem action f) file (nub items)


doItems :: Day -> TodoFile -> [Int] -> (TodoFile, [TodoEvent])
doItems day file = _runEvents . _adjustItems Done (flip markAsDone day) file


undoItems :: TodoFile -> [Int] -> (TodoFile, [TodoEvent])
undoItems file = _runEvents . _adjustItems Undone markAsUndone file


prioritizeItems :: Priority -> TodoFile -> [Int] -> (TodoFile, [TodoEvent])
prioritizeItems p file = _runEvents . _adjustItems Prioritized (flip prioritize p) file


deprioritizeItems :: TodoFile -> [Int] -> (TodoFile, [TodoEvent])
deprioritizeItems file = _runEvents . _adjustItems Deprioritized (flip prioritize noPriority) file
