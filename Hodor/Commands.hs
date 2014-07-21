module Hodor.Commands where

import Control.Arrow (second)
import Control.Monad.Error (Error, ErrorT, MonadError, strMsg, throwError)
import Control.Monad.Reader (ask, ReaderT)
import Control.Monad.Trans (liftIO, MonadIO)
import Data.Foldable (forM_)
import Data.Time (
  Day,
  getZonedTime,
  localDay,
  zonedTimeToLocalTime
  )
import GHC.Exts (sortWith)
import Text.Printf (printf)
import Text.Read (readMaybe)

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
import Hodor.Parser (parseTodoFile)
import Hodor.Types (
  archive,
  doItems,
  DoneResult(..),
  listItems,
  numItems,
  )


type HodorM = ReaderT Config (ErrorT String IO)
type HodorCommand = [String] -> HodorM ()


appName :: String
appName = "HODOR"

appMessage :: String -> String
appMessage = printf "%s: %s" appName


-- XXX: NumberedTodoItem
-- Here we number items according to how they appear, but actually the number
-- is intrinsic to the item, and should probably be associated when parsed.
cmdList :: HodorCommand
cmdList _ = do
  -- ACTION: read file
  todoFile <- fmap todoFilePath ask >>= readTodoFileEx
  -- ACTION: display output
  liftIO $ putStr $ cmdListPure todoFile


cmdListPure :: TodoFile -> String
cmdListPure todoFile =
  let count = numItems todoFile
      todoLines = getTodoLines todoFile
      summary = appMessage $ printf "%d of %d items shown" count count in
  unlines $ todoLines ++ ["--", summary]
  -- XXX: NumberedTodoItem
  where formatOneTodo (i, t) = printf "%02d %s" i t
        sortTodo = sortWith snd
        getTodoLines = map formatOneTodo . sortTodo . map (second unparse) . listItems


cmdAdd :: HodorCommand
cmdAdd args = do
  -- ACTION: get date if we need it
  addDate <- fmap dateOnAdd ask
  day <- case addDate of
    True -> fmap Just (liftIO today)
    False -> return Nothing
  -- ACTION: read file
  todoFile <- fmap todoFilePath ask >>= readTodoFileEx
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
  todoPath <- fmap todoFilePath ask
  todoFile <- readTodoFileEx todoPath
  let (newTodoFile, doneItems) = archive todoFile
      doneString = unlines . map unparse $ doneItems
  donePath <- fmap doneFilePath ask
  liftIO $ appendFile donePath doneString
  liftIO $ replaceFile todoPath (unparse newTodoFile)
  liftIO $ putStr doneString
  liftIO $ putStrLn $ appMessage $ printf "%s archived." todoPath


cmdMarkAsDone :: HodorCommand
cmdMarkAsDone args = do
  items <- getItems args
  day <- liftIO today
  path <- fmap todoFilePath ask
  todoFile <- readTodoFileEx path
  let (newTodoFile, doneItems) = doItems todoFile day items
  liftIO $ replaceFile path $ unparse newTodoFile
  forM_ doneItems (liftIO . putStr . format)
  where
    format (Done i t) = unlines [formatTodo i t,
                                 appMessage $ printf "%d marked as done." i]
    format (AlreadyDone i t) = unlines [formatTodo i t,
                                        appMessage $ printf "%s: %d is already marked done." i]
    format (NoSuchTask i) = appMessage $ printf "%s: No task %d\n" i


{- Utility functions follow -}

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


readTodoFileEx :: (Error e, MonadIO m, MonadError e m) => FilePath -> m TodoFile
readTodoFileEx path = do
  expanded <- liftIO $ expandUser path
  contents <- liftIO $ readFile expanded
  case parseTodoFile expanded contents of
    Left e -> (throwError . strMsg . show) e
    Right r -> return r


today :: IO Day
today = localDay `fmap` zonedTimeToLocalTime `fmap` getZonedTime


-- XXX: Handle 'auto-archive' case
-- XXX: Colorize
-- XXX: Priority list (lsp)
-- XXX: Mark as undone
-- XXX: Filter when listing
-- XXX: Try to get the commands out of IO
--      - they could take Todo, Done and return new Todo, Done
--        - that would avoid cheap writes for 'add' (which currently just append)
--      - they could take Todo, Done and return new Todo, appended Todo, new Done
--        - that could get complicated
--      - some commands do reports too, so need output
--      - perhaps could define some kind of monad that wraps all of this up?
--      - probably best to write more of the commands first
