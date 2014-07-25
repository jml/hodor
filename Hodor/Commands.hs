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
import Hodor.Format (appMessage, Formattable, format)
import Hodor.Functional (andP)
import Hodor.Parser (parseTodoFile)
import Hodor.Types (
  allContexts,
  allProjects,
  archive,
  doItems,
  hasPriority,
  filterItems,
  makePriority,
  noPriority,
  numItems,
  Priority,
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
      messages = [formatStringTodo (count, item), eventAdd count] in
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
  liftIO $ putStrLn $ eventArchived todoPath


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


cmdDeprioritize :: HodorCommand
cmdDeprioritize (i:[]) = do
  (newTodoFile, events) <- liftM (prioritizeItem noPriority) loadTodoFile `ap` getItem i
  replaceTodoFile newTodoFile
  reportEvents events
cmdDeprioritize _        = throwError $ strMsg "Expect ITEM#"


cmdListContexts :: HodorCommand
cmdListContexts _ = do
  liftM allContexts loadTodoFile >>= mapM_ (liftIO . putStrLn . show)


cmdListProjects :: HodorCommand
cmdListProjects _ = do
  liftM allProjects loadTodoFile >>= mapM_ (liftIO . putStrLn . show)


{- Utility functions follow -}

-- XXX: NumberedTodoItem
showTodoList :: TodoFile -> [(Int, TodoItem)] -> String
showTodoList file items = unlines $ concat [formatLines items, ["--", eventItemsShown file items]]


-- XXX: NumberedTodoItem
formatLines :: [(Int, TodoItem)] -> [String]
formatLines =
  map formatStringTodo . sortWith snd . map (second unparse)


-- XXX: NumberedTodoItem
formatStringTodo :: (Int, String) -> String
formatStringTodo (i, t) = printf "%02d %s" i t


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


reportEvents :: (Formattable e) => [e] -> HodorM ()
reportEvents = mapM_ (liftIO . putStr . format)

-- XXX: Make these real events
eventAdd :: Int -> String
eventAdd count = appMessage $ printf "%d added." count

eventArchived :: FilePath -> String
eventArchived file = appMessage $ printf "%s archived." file

-- XXX: NumberedTodoItem
eventItemsShown :: TodoFile -> [(Int, TodoItem)] -> String
eventItemsShown file items = appMessage $ printf "%d of %d items shown" (length items) (numItems file)


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
