module Hodor.Reports where

import Control.Arrow (first)
import Data.Function (on)
import Data.List (groupBy, intercalate, sort)
import Data.Time (Day)


import Hodor (
  Project,
  TodoItem,
  dateCompleted,
  unparse,
  )


-- True when item has been closed on 'day' or after.
closedSince :: Day -> TodoItem -> Bool
closedSince day = maybe True (>= day) . dateCompleted


-- Nicely format a project and its sub-items.
renderProjectAndItems :: (Maybe Project, [TodoItem]) -> String
renderProjectAndItems (p, ts) =
    project ++ "\n" ++ items ++ "\n"
    where
      project = maybe "(no project)" show p
      items = intercalate "\n" (map (("  "++) . unparse) ts)


-- Render TodoItems grouped by projects
projectReview :: [TodoItem] -> String
projectReview = concat . map renderProjectAndItems . groupByProjects


decorate :: (a -> [b]) -> a -> [(Maybe b, a)]
decorate f x = let keys = f x in
               if null keys then [(Nothing, x)]
               else zip (map Just keys) (repeat x)


groupByKeys :: (Ord a, Ord b) => (a -> [b]) -> [a] -> [(Maybe b, [a])]
groupByKeys f = map (first head . unzip) . groupBy (on (==) fst) . sort . concatMap (decorate f)


-- Group TodoItems by project
groupByProjects :: [TodoItem] -> [(Maybe Project, [TodoItem])]
groupByProjects = groupByKeys projects
