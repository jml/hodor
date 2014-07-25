module Hodor.Format where

import Hodor.Types
import Text.Printf


appName :: String
appName = "HODOR"

appMessage :: String -> String
appMessage = printf "%s: %s" appName


class Formattable a where
  format :: a -> String

instance Formattable TodoItem where
  format = unparse

instance Formattable Priority where
  format = init . unparse

instance Formattable TodoEvent where
  format (NoSuchTask i) = appMessage $ printf "No task %d\n" i
  format (TaskChanged e i _ t) =
    unlines [formatTodo i t, appMessage $ formatEvent' e i t]


-- XXX: NumberedTodoItem
formatTodo :: Int -> TodoItem -> String
formatTodo i t = printf "%02d %s" i (unparse t)


formatEvent' :: TaskAction -> Int -> TodoItem -> String
formatEvent' Done i _ = printf "%d marked as done." i
formatEvent' AlreadyDone i _ = printf "%d is already marked done." i
formatEvent' Undone i _ = printf "%d no longer marked as done." i
formatEvent' AlreadyNotDone i _ = printf "%d was already not marked done." i
formatEvent' Prioritized i t = printf "%d prioritized %s." i (format (priority t))
formatEvent' AlreadyPrioritized i t = printf "%d already prioritized %s." i (format (priority t))
formatEvent' (ChangedPriority p) i t = printf "%d re-prioritized from %s to %s." i (format p) (format (priority t))
formatEvent' Deprioritized i _ = printf "%d deprioritized." i
formatEvent' AlreadyDeprioritized i _ = printf "%d is not prioritized." i
