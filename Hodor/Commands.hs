{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Hodor.Commands where

import Control.Applicative (Applicative)
import Control.Monad.Except
import Control.Monad.Reader (ask, MonadReader, ReaderT, runReaderT)
import Data.Maybe (isJust, isNothing)
import Data.Monoid
import Data.Text (pack)
import Data.Time (
  Day,
  getZonedTime,
  localDay,
  zonedTimeToLocalTime
  )
import GHC.Exts (sortWith)
import Rainbow
import Text.Printf (printf)
import Text.Read (readMaybe)
import Text.Regex

import Hodor (
  TodoFile,
  TodoItem,
  unparse
  )
import Hodor.Actions (
  TodoEvent(Archived, Listed),
  addItem,
  appendItem,
  deprioritizeItem,
  doItems,
  prependItem,
  prioritizeItem,
  undoItems,
  )
import Hodor.Config (
  Config,
  colorEnabled,
  todoFilePath,
  dateOnAdd,
  doneFilePath
  )
import Hodor.File (expandUser)
import Hodor.Format (appMessage, Formattable, format, formatTodo)
import Hodor.Functional (andP)
import Hodor.Parser (parseTodoFile)
import Hodor.Types (
  allContexts,
  allProjects,
  archive,
  hasPriority,
  isDone,
  filterItems,
  makePriority,
  numItems,
  Priority,
  )


-- XXX: Revisit error type (maybe we should have something more abstracted
-- than String)
newtype HodorM a = HM { unHM :: ReaderT Config (ExceptT String IO) a }
                   deriving (Functor, Applicative, Monad, MonadIO, MonadReader Config, MonadError String)

type HodorCommand = [String] -> HodorM ()

runHodorCommand :: HodorCommand -> Config -> [String] -> IO (Either String ())
runHodorCommand cmd cfg rest = runExceptT $ runReaderT (unHM (cmd rest)) cfg


cmdList :: HodorCommand
cmdList args =  listItemsCommand (andP matchers)
  where
    matchers = map matcher (filter (not . null) args)
    matcher ('-':arg) = isNothing . matchTodoWithRegex arg
    matcher arg = isJust . matchTodoWithRegex arg
    matchTodoWithRegex arg = matchRegex (mkRegex arg) . unparse


cmdListPriority :: HodorCommand
cmdListPriority _ = listItemsCommand hasPriority


listItemsCommand :: (TodoItem -> Bool) -> HodorM ()
listItemsCommand p = do
  -- ACTION: read file
  todoFile <- loadTodoFile
  let items = filterItems p todoFile
  -- ACTION: display output
  printTodos items
  reportEvents [Listed todoFile (length items)]


cmdAdd :: HodorCommand
cmdAdd args = do
  (item, events) <- liftM addItem getDateAdded `ap` loadTodoFile `ap` (return args)
  -- ACTION: append item
  appendTodoItem item
  reportEvents events


cmdArchive :: HodorCommand
cmdArchive _ = do
  todoFile <- loadTodoFile
  let (newTodoFile, doneItems) = archive todoFile
      doneString = unlines . map unparse $ doneItems
  donePath <- liftM doneFilePath ask
  liftIO $ appendFile donePath doneString
  replaceTodoFile newTodoFile
  liftIO $ putStr doneString
  reportEvents [Archived newTodoFile]


cmdMarkAsDone :: HodorCommand
cmdMarkAsDone args = do
  (newTodoFile, doneItems) <- liftM doItems (liftIO today) `ap` loadTodoFile `ap` getItems args
  replaceTodoFile newTodoFile
  reportEvents doneItems


cmdUndo :: HodorCommand
cmdUndo args = do
  (newTodoFile, doneItems) <- liftM undoItems loadTodoFile `ap` getItems args
  replaceTodoFile newTodoFile
  reportEvents doneItems


cmdPrioritize :: HodorCommand
cmdPrioritize (i:p:[]) = do
  (newTodoFile, events) <- liftM prioritizeItem (getPriority p) `ap` loadTodoFile `ap` (getItem i)
  replaceTodoFile newTodoFile
  reportEvents events
cmdPrioritize _        = throwError "Expect ITEM# PRIORITY"


cmdDeprioritize :: HodorCommand
cmdDeprioritize (i:[]) = do
  (newTodoFile, events) <- liftM deprioritizeItem loadTodoFile `ap` (getItem i)
  replaceTodoFile newTodoFile
  reportEvents events
cmdDeprioritize _        = throwError "Expect ITEM#"


cmdAppend :: HodorCommand
cmdAppend (i:xs) = do
  (newTodoFile, events) <- liftM appendItem (getText xs) `ap` loadTodoFile `ap` (getItem i)
  replaceTodoFile newTodoFile
  reportEvents events
  where getText [] = throwError "Must provide text to append"
        getText ys = return $ unwords ys


cmdPrepend :: HodorCommand
cmdPrepend (i:xs) = do
  (newTodoFile, events) <- liftM prependItem (getText xs) `ap` loadTodoFile `ap` (getItem i)
  replaceTodoFile newTodoFile
  reportEvents events
  where getText [] = throwError "Must provide text to prepend"
        getText ys = return $ unwords ys


cmdListContexts :: HodorCommand
cmdListContexts _ = do
  liftM allContexts loadTodoFile >>= mapM_ (liftIO . putStrLn . format)


cmdListProjects :: HodorCommand
cmdListProjects _ = do
  liftM allProjects loadTodoFile >>= mapM_ (liftIO . putStrLn . format)


{- Utility functions follow -}

-- XXX: NumberedTodoItem
printTodos :: [(Int, TodoItem)] -> HodorM ()
printTodos items = do
  useColor <- liftM colorEnabled ask
  case useColor of
    False -> liftIO . putStr . unlines . map (uncurry formatTodo) . sortWith snd $ items
    True -> _printTodosColor items


_printTodosColor :: [(Int, TodoItem)] -> HodorM ()
_printTodosColor = liftIO . mapM_ (putChunkLn . uncurry _colorizeTodo) . sortWith snd


_colorizeTodo :: Int -> TodoItem -> Chunk
_colorizeTodo i t
  | isDone t      = todoText <> fore grey <> fore magenta8
  | hasPriority t = todoText <> fore white <> bold
  | otherwise     = todoText <> fore white
  where todoText = fromText $ pack $ formatTodo i t


getItems :: MonadError String m => [String] -> m [Int]
getItems [] = throwError "No items specified"
getItems xs = mapM getItem xs


getItem :: MonadError String m => String -> m Int
getItem x =
  case readMaybe x of
    Just i -> return i
    Nothing -> throwError $ "Invalid number: " ++ x


getPriority :: MonadError String m => String -> m Priority
getPriority (c:[]) =
  case makePriority c of
    Just p  -> return p
    Nothing -> getPriority []
getPriority x      = throwError $ "Invalid priority: " ++ x


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
    Left e -> (throwError . show) e
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
