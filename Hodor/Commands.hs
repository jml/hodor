module Hodor.Commands where

import Control.Arrow (second)
import Control.Monad (liftM)
import Control.Monad.Error (Error, ErrorT, MonadError, runErrorT, strMsg, throwError)
import Control.Monad.Reader (ask, ReaderT, runReaderT)
import Control.Monad.Trans (liftIO, MonadIO)
import Data.Foldable (concatMap, forM_)
import Data.List (nub, sort)
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
  allItems,
  contexts,
  archive,
  doItems,
  DoneResult(..),
  hasPriority,
  filterItems,
  numItems,
  projects
  )


-- XXX: Revisit error type (maybe we should have something more abstracted
-- than String)
type HodorM = ReaderT Config (ErrorT String IO)
-- XXX: Try to wrap this up in a newtype, for greater encapsulation
type HodorCommand = [String] -> HodorM ()

runHodorCommand :: HodorCommand -> Config -> [String] -> IO (Either String ())
runHodorCommand cmd cfg rest = runErrorT $ runReaderT (cmd rest) cfg

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
    -- XXX: I don't know whether it helps (i.e. improves performance) if we
    -- run mkRegex over the list first. It's possible that this means we only
    -- compile every regex once, whereas otherwise we'd compile them once per
    -- item in the todo list. I also don't know how to find out.
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
  -- ACTION: get date if we need it
  addDate <- fmap dateOnAdd ask
  day <- case addDate of
    True -> fmap Just (liftIO today)
    False -> return Nothing
  -- ACTION: read file
  todoFile <- loadTodoFile
  let (item, output) = cmdAddPure todoFile day args
  -- ACTION: append item
  todoFileName <- fmap todoFilePath ask
  liftIO $ appendFile todoFileName $ item
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
  todoPath <- fmap todoFilePath ask
  donePath <- fmap doneFilePath ask
  liftIO $ appendFile donePath doneString
  liftIO $ replaceFile todoPath (unparse newTodoFile)
  liftIO $ putStr doneString
  liftIO $ putStrLn $ appMessage $ printf "%s archived." todoPath


cmdMarkAsDone :: HodorCommand
cmdMarkAsDone args = do
  items <- getItems args
  day <- liftIO today
  todoFile <- loadTodoFile
  let (newTodoFile, doneItems) = doItems todoFile day items
  path <- fmap todoFilePath ask
  liftIO $ replaceFile path $ unparse newTodoFile
  forM_ doneItems (liftIO . putStr . format)
  where
    format (Done i t) = unlines [formatTodo i t,
                                 appMessage $ printf "%d marked as done." i]
    format (AlreadyDone i t) = unlines [formatTodo i t,
                                        appMessage $ printf "%s: %d is already marked done." i]
    format (NoSuchTask i) = appMessage $ printf "%s: No task %d\n" i


cmdListContexts :: HodorCommand
cmdListContexts _ = do
  liftM getAllContexts loadTodoFile >>= mapM_ (liftIO . putStrLn . show)
  where getAllContexts = nub . sort . Data.Foldable.concatMap contexts . allItems


cmdListProjects :: HodorCommand
cmdListProjects _ = do
  liftM getAllProjects loadTodoFile >>= mapM_ (liftIO . putStrLn . show)
  where getAllProjects = nub . sort . Data.Foldable.concatMap projects . allItems



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


getItems :: (Error e, MonadError e m) => [String] -> m [Int]
getItems [] = throwError $ strMsg "No items specified"
getItems xs =
  mapM (\x ->
         case readMaybe x of
           Just i -> return i
           Nothing -> throwError $ strMsg $ "Invalid number: " ++ x) xs


-- XXX: Making this separate because an atomic write would be better, but I
-- can't be bothered looking up how to do that right now.
replaceFile :: FilePath -> String -> IO ()
replaceFile = writeFile


loadTodoFile :: HodorM TodoFile
loadTodoFile = do
  path <- fmap todoFilePath ask
  expanded <- liftIO $ expandUser path
  contents <- liftIO $ readFile expanded
  case parseTodoFile expanded contents of
    Left e -> (throwError . strMsg . show) e
    Right r -> return r


today :: IO Day
today = localDay `fmap` zonedTimeToLocalTime `fmap` getZonedTime


-- XXX: Handle 'auto-archive' case
-- XXX: Colorize
-- XXX: Mark as undone
-- XXX: Try to get the commands out of IO
--      - they could take Todo, Done and return new Todo, Done
--        - that would avoid cheap writes for 'add' (which currently just append)
--      - they could take Todo, Done and return new Todo, appended Todo, new Done
--        - that could get complicated
--      - some commands do reports too, so need output
--      - perhaps could define some kind of monad that wraps all of this up?
--      - probably best to write more of the commands first
