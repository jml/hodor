module Hodor (
  contexts,
  dateCreated,
  dateCompleted,
  description,
  groupByProjects,
  parseTodoFile,
  priority,
  projects,
  todoFileItems,
  todoFileName,
  Project,
  Context,
  Priority,
  TodoItem,
  TodoFile,
  ) where

import Control.Arrow (first)
import Data.Function (on)
import Data.List (groupBy, sort)

import Hodor.Parser (parseTodoFile)
import Hodor.Types (
  Context,
  Priority,
  Project,
  TodoFile,
  TodoItem,
  contexts,
  dateCompleted,
  dateCreated,
  description,
  priority,
  projects,
  todoFileItems,
  todoFileName)


decorate :: (a -> [b]) -> a -> [(Maybe b, a)]
decorate f x = let keys = f x in
               if null keys then [(Nothing, x)]
               else zip (map Just keys) (repeat x)

groupByKeys :: (Ord a, Ord b) => (a -> [b]) -> [a] -> [(Maybe b, [a])]
groupByKeys f = map (first head . unzip) . groupBy (on (==) fst) . sort . concatMap (decorate f)


groupByProjects :: [TodoItem] -> [(Maybe Project, [TodoItem])]
groupByProjects = groupByKeys projects
