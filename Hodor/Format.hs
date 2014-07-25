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
  format (TaskChanged e i o t) =
    unlines [formatTodo i t, appMessage $ formatEvent' e i o t]


-- XXX: NumberedTodoItem
formatTodo :: Int -> TodoItem -> String
formatTodo i t = printf "%02d %s" i (unparse t)


formatEvent' :: TaskAction -> Int -> TodoItem -> TodoItem -> String
formatEvent' Done i o _ | isDone o  = printf "%d is already marked done." i
                        | otherwise = printf "%d marked as done." i
formatEvent' AlreadyDone i _ _ = printf "%d is already marked done." i

formatEvent' Undone i o _ | isDone o =  printf "%d no longer marked as done." i
                          | otherwise = printf "%d was already not marked done." i
formatEvent' AlreadyNotDone i _ _ = printf "%d was already not marked done." i

formatEvent' Prioritized i o t =
  if hasPriority o
  then if oldPriority == newPriority
       then printf "%d already prioritized %s." i (format oldPriority)
       else printf "%d re-prioritized from %s to %s." i (format oldPriority) (format newPriority)
  else printf "%d prioritized %s." i (format newPriority)
  where oldPriority = priority o
        newPriority = priority t
formatEvent' AlreadyPrioritized i o _ = printf "%d already prioritized %s." i (format (priority o))
formatEvent' (ChangedPriority p) i o _ = printf "%d re-prioritized from %s to %s." i (format p) (format (priority o))

formatEvent' Deprioritized i o _ | hasPriority o = printf "%d deprioritized." i
                                 | otherwise     = printf "%d is not prioritized." i
formatEvent' AlreadyDeprioritized i _ _ = printf "%d is not prioritized." i
