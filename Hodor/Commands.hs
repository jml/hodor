{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Hodor.Commands where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.Reader (ask, MonadReader, ReaderT, runReaderT)
import Data.List (intersperse)
import Data.Maybe (isJust, isNothing, fromMaybe)
import Data.Monoid
import Data.Text (pack)
import Data.Time (
  Day,
  getZonedTime,
  localDay,
  showGregorian,
  zonedTimeToLocalTime
  )
import GHC.Exts (sortWith)
import Rainbow
import Text.Printf (printf)
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
  dateCompleted,
  dateCreated,
  description,
  getPriority,
  hasPriority,
  isDone,
  filterItems,
  numItems,
  Priority,
  priority,
  )


-- XXX: Revisit error type (maybe we should have something more abstracted
-- than String)
newtype HodorM a =
  HM { unHM :: ReaderT Config (ExceptT String IO) a }
  deriving (
    Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader Config
    , MonadError String)


data Command =
  ListCommand [String] |
  ListPriorityCommand |
  AddCommand [String] |
  DoCommand [Int] |
  UndoCommand [Int] |
  ArchiveCommand |
  ListContextCommand |
  ListProjectCommand |
  PrioritizeCommand Int Priority |
  DeprioritizeCommand Int |
  AppendCommand Int [String] |
  PrependCommand Int [String]
  deriving (Show)


runHodorM :: HodorM a -> Config -> IO (Either String a)
runHodorM x cfg = runExceptT $ runReaderT (unHM x) cfg


dispatchCommand :: Command -> HodorM ()
dispatchCommand (ListCommand args)           = cmdList args
dispatchCommand ListPriorityCommand          = cmdListPriority
dispatchCommand (AddCommand args)            = cmdAdd args
dispatchCommand (DoCommand items)            = cmdMarkAsDone items
dispatchCommand (UndoCommand items)          = cmdUndo items
dispatchCommand ArchiveCommand               = cmdArchive
dispatchCommand ListContextCommand           = cmdListContexts
dispatchCommand ListProjectCommand           = cmdListProjects
dispatchCommand (PrioritizeCommand item pri) = cmdPrioritize item pri
dispatchCommand (DeprioritizeCommand item)   = cmdDeprioritize item
dispatchCommand (AppendCommand item wds)     = cmdAppend item wds
dispatchCommand (PrependCommand item wds)    = cmdPrepend item wds


cmdList :: [String] -> HodorM ()
cmdList args =  listItemsCommand (andP matchers)
  where
    matchers = map matcher (filter (not . null) args)
    matcher ('~':arg) = isNothing . matchTodoWithRegex arg
    matcher arg = isJust . matchTodoWithRegex arg
    matchTodoWithRegex arg = matchRegex (mkRegex arg) . unparse


cmdListPriority :: HodorM ()
cmdListPriority = listItemsCommand hasPriority


listItemsCommand :: (TodoItem -> Bool) -> HodorM ()
listItemsCommand p = do
  todoFile <- loadTodoFile
  let items = filterItems p todoFile
  printTodos items
  reportEvents [Listed todoFile (length items)]


cmdAdd :: [String] -> HodorM ()
cmdAdd args = do
  (item, events) <- addItem <$> getDateAdded <*> loadTodoFile <*> (pure args)
  appendTodoItem item
  reportEvents events


cmdArchive :: HodorM ()
cmdArchive = do
  todoFile <- loadTodoFile
  let (newTodoFile, doneItems) = archive todoFile
      doneString = unlines . map unparse $ doneItems
  donePath <- doneFilePath <$> ask
  liftIO $ appendFile donePath doneString
  replaceTodoFile newTodoFile
  liftIO $ putStr doneString
  reportEvents [Archived newTodoFile]


cmdMarkAsDone :: [Int] -> HodorM ()
cmdMarkAsDone items = do
  (newTodoFile, doneItems) <- doItems <$> (liftIO today) <*> loadTodoFile <*> (pure items)
  replaceTodoFile newTodoFile
  reportEvents doneItems


cmdUndo :: [Int] -> HodorM ()
cmdUndo items = do
  (newTodoFile, doneItems) <- undoItems <$> loadTodoFile <*> (pure items)
  replaceTodoFile newTodoFile
  reportEvents doneItems


cmdPrioritize :: Int -> Priority -> HodorM ()
cmdPrioritize item p = do
  (newTodoFile, events) <- prioritizeItem <$> (pure p) <*> loadTodoFile <*> (pure item)
  replaceTodoFile newTodoFile
  reportEvents events


cmdDeprioritize :: Int -> HodorM ()
cmdDeprioritize item = do
  (newTodoFile, events) <- deprioritizeItem <$> loadTodoFile <*> (pure item)
  replaceTodoFile newTodoFile
  reportEvents events


cmdAppend :: Int -> [String] -> HodorM ()
cmdAppend item xs = do
  (newTodoFile, events) <- appendItem <$> (getText xs) <*> loadTodoFile <*> (pure item)
  replaceTodoFile newTodoFile
  reportEvents events
  where getText [] = throwError "Must provide text to append"
        getText ys = return $ unwords ys


cmdPrepend :: Int -> [String] -> HodorM ()
cmdPrepend item xs = do
  (newTodoFile, events) <- prependItem <$> (getText xs) <*> loadTodoFile <*> (pure item)
  replaceTodoFile newTodoFile
  reportEvents events
  where getText [] = throwError "Must provide text to prepend"
        getText ys = return $ unwords ys


cmdListContexts :: HodorM ()
cmdListContexts = do
  allContexts <$> loadTodoFile >>= mapM_ (liftIO . putStrLn . format)


cmdListProjects :: HodorM ()
cmdListProjects = do
  allProjects <$> loadTodoFile >>= mapM_ (liftIO . putStrLn . format)


{- Utility functions follow -}

-- XXX: NumberedTodoItem
printTodos :: [(Int, TodoItem)] -> HodorM ()
printTodos items = do
  useColor <- colorEnabled <$> ask
  case useColor of
    False -> liftIO . putStr . unlines . map (uncurry formatTodo) . sortWith snd $ items
    True -> _printTodosColor items


_printTodosColor :: [(Int, TodoItem)] -> HodorM ()
_printTodosColor todos = do
  term <- liftIO termFromEnv
  liftIO . mapM_ (putChunks term . uncurry _colorizeTodo) . sortWith snd $ todos


_colorizeTodo :: Int -> TodoItem -> [Chunk]
_colorizeTodo i t
  | isDone t      = [mconcat todoText <> fore grey]
  | hasPriority t = map (<> bold) todoText
  | otherwise     = todoText
  where todoText = _displayTodo i t



-- XXX: There's probably a really nice table display library out there.
-- XXX: make it possible to configure colours for specific contexts and projects
-- XXX: The structure of this is kind of hideous: clean it up.
_displayTodo :: Int -> TodoItem -> [Chunk]
_displayTodo i t =
  case (dateCompleted t) of
    Nothing -> [
      fromText $ pack $ printf "%02d " i,
      priorityBit,
      dateBit, " "] ++ descriptionChunks ++ ["\n"]
    Just completed -> [
      fromText $ pack $ printf "%02d  x  " i,
      dateChunk completed,
      " "] ++ descriptionChunks ++ ["\n"]
  where
    dateChunk = fromText . pack . showGregorian
    dateBit = fromMaybe "          " (dateChunk <$> dateCreated t)
    priorityBit =
      case getPriority (priority t) of
        Nothing  -> "    "
        Just 'A' -> "(A) " <> fore red
        Just x   -> fromText $ pack $ '(':x:") "
    descriptionChunks = intersperse (fromText " ") $ map chunkWord (words $ description t)
    chunkWord xs@('+':_) = (fromText $ pack xs) <> fore brightBlue
    chunkWord xs@('@':_) = (fromText $ pack xs) <> fore green
    chunkWord xs         = fromText $ pack xs


-- XXX: Making this separate because an atomic write would be better, but I
-- can't be bothered looking up how to do that right now.
replaceFile :: FilePath -> String -> IO ()
replaceFile = writeFile


loadTodoFile :: HodorM TodoFile
loadTodoFile = do
  path <- todoFilePath <$> ask
  expanded <- liftIO $ expandUser path
  contents <- liftIO $ readFile expanded
  case parseTodoFile expanded contents of
    Left e -> (throwError . show) e
    Right r -> return r


replaceTodoFile :: TodoFile -> HodorM ()
replaceTodoFile newTodoFile = do
  path <- todoFilePath <$> ask
  liftIO $ replaceFile path $ unparse newTodoFile


appendTodoItem :: String -> HodorM ()
appendTodoItem item = do
  todoFileName <- todoFilePath <$> ask
  liftIO $ appendFile todoFileName $ item


reportEvents :: (Formattable e) => [e] -> HodorM ()
reportEvents = mapM_ (liftIO . putStr . format)

-- XXX: NumberedTodoItem
eventItemsShown :: TodoFile -> [(Int, TodoItem)] -> String
eventItemsShown file items = appMessage $ printf "%d of %d items shown" (length items) (numItems file)


getDateAdded :: HodorM (Maybe Day)
getDateAdded = do
  addDate <- dateOnAdd <$> ask
  case addDate of
    True -> Just <$> (liftIO today)
    False -> pure Nothing


today :: IO Day
today = localDay <$> zonedTimeToLocalTime <$> getZonedTime


-- XXX: Handle 'auto-archive' case
-- XXX: Try to get the commands out of IO
--      - they could take Todo, Done and return new Todo, Done
--        - that would avoid cheap writes for 'add' (which currently just append)
--      - they could take Todo, Done and return new Todo, appended Todo, new Done
--        - that could get complicated
--      - some commands do reports too, so need output
--      - perhaps could define some kind of monad that wraps all of this up?
--      - probably best to write more of the commands first

