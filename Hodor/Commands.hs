{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Hodor.Commands where

import Control.Arrow (second)
import Control.Monad (ap, liftM)
import Control.Monad.Error (Error, ErrorT, MonadError, runErrorT, strMsg, throwError)
import Control.Monad.Reader (ask, MonadReader, ReaderT, runReaderT)
import Control.Monad.Trans (liftIO, MonadIO)
import Data.Maybe (isJust, isNothing)
import Data.Time (
  Day,
  getZonedTime,
  localDay,
  zonedTimeToLocalTime
  )
import GHC.Exts (sortWith)
import Text.Printf (printf)
import Text.Read (readMaybe)
import Text.Regex

import Hodor (
  TodoFile,
  TodoItem,
  unparse
  )
import Hodor.Config (
  Config,
  todoFilePath,
  dateOnAdd,
  doneFilePath
  )
import Hodor.File (expandUser)
import Hodor.Functional (andP)
import Hodor.Parser (parseTodoFile)
import Hodor.Types (
  allContexts,
  allProjects,
  archive,
  doItems,
  TaskAction(..),
  TodoEvent(..),
  hasPriority,
  filterItems,
  makePriority,
  numItems,
  Priority,
  priority,
  prioritizeItem,
  undoItems,
  )


-- XXX: Revisit error type (maybe we should have something more abstracted
-- than String)
newtype HodorM a = HM { unHM :: ReaderT Config (ErrorT String IO) a }
                   deriving (Monad, MonadIO, MonadReader Config, MonadError String)

type HodorCommand = [String] -> HodorM ()

runHodorCommand :: HodorCommand -> Config -> [String] -> IO (Either String ())
runHodorCommand cmd cfg rest = runErrorT $ runReaderT (unHM (cmd rest)) cfg


appName :: String
appName = "HODOR"

appMessage :: String -> String
appMessage = printf "%s: %s" appName


cmdList :: HodorCommand
cmdList args =  listItemsCommand (andP matchers)
  where
    matcher ('-':arg) = isNothing . matchTodoWithRegex arg
    matcher arg = isJust . matchTodoWithRegex arg
    matchTodoWithRegex arg = matchRegex (mkRegex arg) . unparse
    matchers = map matcher (filter (not . null) args)


cmdListPriority :: HodorCommand
cmdListPriority _ = listItemsCommand hasPriority


listItemsCommand :: (TodoItem -> Bool) -> HodorM ()
listItemsCommand p = do
  -- ACTION: read file
  todoFile <- loadTodoFile
  -- ACTION: display output
  liftIO $ putStr $ showTodoList todoFile (filterItems p todoFile)


cmdAdd :: HodorCommand
cmdAdd args = do
  (item, output) <- liftM cmdAddPure loadTodoFile `ap` getDateAdded `ap` (return args)
  -- ACTION: append item
  appendTodoItem item
  -- ACTION: display output
  liftIO $ putStr $ output


cmdAddPure :: TodoFile -> Maybe Day -> [String] -> (String, String)
cmdAddPure todoFile (Just day) args = cmdAddPure todoFile Nothing (show day:args)
cmdAddPure todoFile Nothing args =
  let item = unwords args ++ "\n"
      count = numItems todoFile + 1
      -- XXX: NumberedTodoItem
      messages = [printf "%02d %s" count item, appMessage $ printf "%d added." count] in
  (item, unlines messages)


cmdArchive :: HodorCommand
cmdArchive _ = do
  todoFile <- loadTodoFile
  let (newTodoFile, doneItems) = archive todoFile
      doneString = unlines . map unparse $ doneItems
  todoPath <- liftM todoFilePath ask
  donePath <- liftM doneFilePath ask
  liftIO $ appendFile donePath doneString
  replaceTodoFile newTodoFile
  liftIO $ putStr doneString
  liftIO $ putStrLn $ appMessage $ printf "%s archived." todoPath


cmdMarkAsDone :: HodorCommand
cmdMarkAsDone args = do
  (newTodoFile, doneItems) <- liftM doItems loadTodoFile `ap` liftIO today `ap` getItems args
  replaceTodoFile newTodoFile
  reportEvents doneItems


cmdUndo :: HodorCommand
cmdUndo args = do
  (newTodoFile, doneItems) <- liftM undoItems loadTodoFile `ap` getItems args
  replaceTodoFile newTodoFile
  reportEvents doneItems


cmdPrioritize :: HodorCommand
cmdPrioritize (i:p:[]) = do
  (newTodoFile, events) <- liftM prioritizeItem (getPriority p) `ap` loadTodoFile `ap` getItem i
  replaceTodoFile newTodoFile
  reportEvents events
cmdPrioritize _        = throwError $ strMsg "Expect ITEM# PRIORITY"


cmdListContexts :: HodorCommand
cmdListContexts _ = do
  liftM allContexts loadTodoFile >>= mapM_ (liftIO . putStrLn . show)


cmdListProjects :: HodorCommand
cmdListProjects _ = do
  liftM allProjects loadTodoFile >>= mapM_ (liftIO . putStrLn . show)


{- Utility functions follow -}

-- XXX: NumberedTodoItem
showTodoList :: TodoFile -> [(Int, TodoItem)] -> String
showTodoList file items = unlines $ concat [formatLines items, ["--", getListSummary file items]]


-- XXX: NumberedTodoItem
formatLines :: [(Int, TodoItem)] -> [String]
formatLines =
  map formatOneTodo . sortWith snd . map (second unparse)
  where formatOneTodo (i, t) = printf "%02d %s" i t


-- XXX: NumberedTodoItem
getListSummary :: TodoFile -> [(Int, TodoItem)] -> String
getListSummary file items = appMessage $ printf "%d of %d items shown" (length items) (numItems file)


-- XXX: NumberedTodoItem
formatTodo :: Int -> TodoItem -> String
formatTodo i t = printf "%02d %s" i (unparse t)


formatEvent :: TodoEvent -> String
formatEvent (NoSuchTask i) = appMessage $ printf "No task %d\n" i
formatEvent (TaskChanged i t e) =
  unlines [formatTodo i t, appMessage $ formatEvent' e i t]

formatEvent' :: TaskAction -> Int -> TodoItem -> String
formatEvent' Done i _ = printf "%d marked as done." i
formatEvent' AlreadyDone i _ = printf "%d is already marked done." i
formatEvent' Undone i _ = printf "%d no longer marked as done." i
formatEvent' AlreadyNotDone i _ = printf "%d was already not marked done." i
formatEvent' Prioritized i t = printf "%d prioritized %s." i (formatPriority (priority t))
formatEvent' AlreadyPrioritized i t = printf "%d already prioritized %s." i (formatPriority (priority t))
formatEvent' (ChangedPriority p) i t = printf "%d re-prioritized from %s to %s." i (formatPriority (priority t)) (formatPriority p)


formatPriority = init . unparse

getItems :: (Error e, MonadError e m) => [String] -> m [Int]
getItems [] = throwError $ strMsg "No items specified"
getItems xs = mapM getItem xs


getItem :: (Error e, MonadError e m) => String -> m Int
getItem x =
  case readMaybe x of
    Just i -> return i
    Nothing -> throwError $ strMsg $ "Invalid number: " ++ x


getPriority :: (Error e, MonadError e m) => String -> m Priority
getPriority (c:[]) =
  case makePriority c of
    Just p  -> return p
    Nothing -> getPriority []
getPriority x      = throwError $ strMsg $ "Invalid priority: " ++ x


-- XXX: Making this separate because an atomic write would be better, but I
-- can't be bothered looking up how to do that right now.
replaceFile :: FilePath -> String -> IO ()
replaceFile = writeFile


loadTodoFile :: HodorM TodoFile
loadTodoFile = do
  path <- liftM todoFilePath ask
  expanded <- liftIO $ expandUser path
  contents <- liftIO $ readFile expanded
  case parseTodoFile expanded contents of
    Left e -> (throwError . strMsg . show) e
    Right r -> return r


replaceTodoFile :: TodoFile -> HodorM ()
replaceTodoFile newTodoFile = do
  path <- liftM todoFilePath ask
  liftIO $ replaceFile path $ unparse newTodoFile


appendTodoItem :: String -> HodorM ()
appendTodoItem item = do
  todoFileName <- liftM todoFilePath ask
  liftIO $ appendFile todoFileName $ item


reportEvents :: [TodoEvent] -> HodorM ()
reportEvents = mapM_ (liftIO . putStr . formatEvent)


getDateAdded :: HodorM (Maybe Day)
getDateAdded = do
  addDate <- liftM dateOnAdd ask
  case addDate of
    True -> liftM Just (liftIO today)
    False -> return Nothing


today :: IO Day
today = localDay `fmap` zonedTimeToLocalTime `fmap` getZonedTime


-- XXX: Handle 'auto-archive' case
-- XXX: Colorize
-- XXX: Try to get the commands out of IO
--      - they could take Todo, Done and return new Todo, Done
--        - that would avoid cheap writes for 'add' (which currently just append)
--      - they could take Todo, Done and return new Todo, appended Todo, new Done
--        - that could get complicated
--      - some commands do reports too, so need output
--      - perhaps could define some kind of monad that wraps all of this up?
--      - probably best to write more of the commands first
