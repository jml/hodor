{-# LANGUAGE FlexibleContexts #-}

module Hodor.CommandLine where

import Control.Monad (liftM, forM_)
import Control.Monad.Error (Error, ErrorT, mapErrorT, MonadError, runErrorT, strMsg, throwError)
import Control.Monad.Reader (ask, ReaderT, runReaderT)
import Control.Monad.Trans (liftIO)
import Data.List (intercalate)
import qualified Data.Map as M
import Data.Maybe ( fromMaybe )
import Data.Time (
  Day,
  getZonedTime,
  localDay,
  zonedTimeToLocalTime
  )
import GHC.Exts (sortWith)
import System.Console.GetOpt
import System.Environment (getArgs)
import Text.Printf (printf)
import Text.Read ( readMaybe )


import Hodor (
  TodoFile
  , TodoItem
  , markAsDone
  , todoFileItems
  , unparse
  )
import Hodor.File (expandUser)
import Hodor.Functional (enumerate, onLeft)
import Hodor.Parser (ParseError, parseTodoFile)
import Hodor.Types (onTodos)

data Config = Config {
  todoFilePath :: FilePath,
  doneFilePath :: FilePath,
  dateOnAdd :: Bool
} deriving (Show)


data Flag = TodoFile FilePath | DoneFile FilePath


data UserError = UserError String
                 deriving (Eq, Show)


instance Error UserError where
  strMsg = UserError


options :: [OptDescr Flag]
options =
    [ Option ['t'] ["todo-file"] (OptArg tFile "FILE") "location of todo file"
    , Option ['d'] ["done-file"] (OptArg dFile "FILE") "location of done file"
    ]


defaultTodoFile, defaultDoneFile :: FilePath
defaultTodoFile = "/Users/jml/.hodor/todo.txt"
defaultDoneFile = "/Users/jml/.hodor/done.txt"

defaultConfig :: Config
defaultConfig = Config { todoFilePath = defaultTodoFile,
                         doneFilePath = defaultDoneFile,
                         dateOnAdd = True }

today :: IO Day
today = localDay `fmap` zonedTimeToLocalTime `fmap` getZonedTime


-- XXX: It's a bit crappy having these defaults specified twice over
tFile,dFile :: Maybe String -> Flag
tFile = TodoFile . fromMaybe defaultTodoFile
dFile = DoneFile  . fromMaybe defaultDoneFile


hodorOpts :: [String] -> Either UserError ([Flag], [String])
hodorOpts argv =
  case getOpt Permute options argv of
    (o,n,[]  ) -> return (o,n)
    (_,_,errs) -> throwError (usageError errs)


usageError :: [String] -> UserError
usageError errs = UserError (concat errs ++ usageInfo header options)
                  where header = "Usage: hodor [OPTION...] "


-- XXX: Is there a better way of doing this transformation?
-- XXX: Try out lenses
getConfiguration :: [Flag] -> Config
getConfiguration ((TodoFile path):xs) =
  let config = (getConfiguration xs)
  in config { todoFilePath = path }
getConfiguration ((DoneFile path):xs) =
  let config = (getConfiguration xs)
  in config { doneFilePath = path }
getConfiguration [] = defaultConfig


-- XXX: See hodorOpts for possible change to type signature
--readTodoFileEx :: FilePath -> ErrorT ParseError IO TodoFile
readTodoFileEx path = do
  expanded <- liftIO $ expandUser path
  contents <- liftIO $ readFile expanded
  case parseTodoFile expanded contents of
    Left e -> throwError e
    Right r -> return r



appName :: String
appName = "HODOR"


type HodorCommand = ErrorT ParseError (ReaderT Config IO)


-- Here we number items according to how they appear, but actually the number
-- is intrinsic to the item, and should probably be associated when parsed.
cmdList :: [String] -> HodorCommand ()
cmdList _ = do
  -- ACTION: read file
  todoFile <- fmap todoFilePath ask >>= readTodoFileEx
  -- ACTION: display output
  liftIO $ putStr $ cmdListPure todoFile


cmdListPure :: TodoFile -> String
cmdListPure todoFile =
  let items = todoFileItems todoFile
      count = length items
      todoLines = map formatTodo $ sortTodo $ enumerate $ map unparse $ items
      summary = printf "%s: %d of %d items shown" appName count count in
  unlines $ todoLines ++ ["--", summary]
  where formatTodo (i, t) = printf "%02d %s" i t
        sortTodo = sortWith snd


cmdAdd :: [String] -> HodorCommand ()
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
  liftIO $ putStrLn $ output


cmdAddPure :: TodoFile -> Maybe Day -> [String] -> (String, String)
cmdAddPure todoFile (Just day) args = cmdAddPure todoFile Nothing (show day:args)
cmdAddPure todoFile Nothing args =
  let item = intercalate " " args ++ "\n"
      count = (length $ todoFileItems todoFile) + 1
      message = printf "%02d %s\n%s: %d added." count item appName count in
  (item, message)


cmdMarkAsDone :: [String] -> HodorCommand ()
cmdMarkAsDone args = do
  items <- wrapError (strMsg . show) (getItems args)
  day <- liftIO today
  path <- fmap todoFilePath ask
  todoFile <- readTodoFileEx path
  let (newTodoFile, doneItems) = cmdMarkAsDonePure todoFile day items
  liftIO $ replaceFile path $ unparse newTodoFile
  forM_ doneItems (\(i, t) -> liftIO $
                              do putStrLn $ printf "%02d %s" i (unparse t)
                                 putStrLn $ printf "%s: %d marked as done." appName i)
  -- XXX: Handle 'auto-archive' case

-- XXX: Say 'HODOR: NN is already marked done.' if already done
-- XXX: Say 'HODOR: No task NN.' if NN out of range



-- XXX: Hideously under-performant
cmdMarkAsDonePure :: TodoFile -> Day -> [Integer] -> (TodoFile, [(Integer, TodoItem)])
cmdMarkAsDonePure todoFile day nums =
  let indexed = enumerate . todoFileItems $ todoFile
      chosen = selectIndices nums indexed
      updated = map (\(i, x) -> (i, markAsDone x day)) chosen
  in (onTodos (flip replaceItems updated) todoFile, updated)

getItems :: (MonadError UserError m) => [String] -> m [Int]
-- XXX: Change this to use strMsg?
getItems [] = throwError $ UserError "No items specified"
getItems xs =
  mapM (\x ->
         case readMaybe x of
           Just i -> return i
           Nothing -> throwError $ UserError $ "Invalid number: " ++ x) xs

-- XXX: Strikes me that there ought to be a way to use 'do' notation for Maybe
-- operations and then turn it into an Either with an error message at the
-- very end.

-- XXX: Hoogle 'Either a b -> Maybe b' and 'Maybe b -> a -> Either a b'

-- XXX: Making this separate because an atomic write would be better, but I
-- can't be bothered looking up how to do that right now.
replaceFile :: FilePath -> String -> IO ()
replaceFile = writeFile



-- XXX: What if indices are negative? What if they are out of range?

-- XXX: Currently 1-based (that's what enumerate does). Ideally would be
-- 0-based and we'd transform.

-- XXX: Currently O(N * M), worst case O(N ** 2). Interesting exercise to
-- rewrite as performant with lists, but maybe better just to replace core
-- type with Vector?

-- XXX: Move these to Types once I really understand them.

selectIndices :: [Integer] -> [a] -> [a]
selectIndices is xs = [ x | (i, x) <- enumerate xs, i `elem` is ]

replaceItems :: [a] -> [(Integer, a)] -> [a]
replaceItems xs is =
  map doSelected . enumerate $ xs
  where doSelected (i, x) = maybe x id (lookup i is)

onIndices :: (a -> a) -> [Integer] -> [a] -> [a]
onIndices f is =
  map doSelected . enumerate
  where doSelected (i, x) | i `elem` is = f x
                          | otherwise   = x


-- XXX: Make tests for this stuff, dammit (see 'get out of IO' below)
-- XXX: Add some QuickCheck tests
-- XXX: Handle 'auto-archive' case
-- XXX: Colorize
-- XXX: Priority list (lsp)
-- XXX: Mark as undone
-- XXX: Filter when listing
-- XXX: External config file (yaml?)
-- XXX: Look into better idioms for config
--      - Reader monad?
--      - lenses?
-- XXX: Try to get the commands out of IO
--      - they could take Todo, Done and return new Todo, Done
--        - that would avoid cheap writes for 'add' (which currently just append)
--      - they could take Todo, Done and return new Todo, appended Todo, new Done
--        - that could get complicated
--      - some commands do reports too, so need output
--      - perhaps could define some kind of monad that wraps all of this up?
--      - probably best to write more of the commands first

-- TODO: Archive


commands :: M.Map String ([String] -> HodorCommand ())
commands = M.fromList [
  ("list", cmdList)
  , ("ls",   cmdList)
  , ("add",  cmdAdd)
  , ("do", cmdMarkAsDone)
  ]


wrapError :: Monad m => (e -> e') -> ErrorT e m a -> ErrorT e' m a
wrapError = mapErrorT . liftM . onLeft


runHodorCommand :: Config -> HodorCommand a -> IO (Either String a)
runHodorCommand cfg cmd =
  runReaderT (runErrorT $ wrapError show $ cmd) cfg


getCommand (name:rest) =
  case M.lookup name commands of
    Just command -> return (command, rest)
    Nothing -> throwError $ UserError (concat ["No such command: ", name, "\n"])
-- XXX: Make the default command configurable
getCommand [] = return (cmdList, [])


main :: IO ()
main = do
  argv <- getArgs
  -- XXX: Classic staircase. Get rid of it once other things have settled
  -- down.
  case hodorOpts argv of
    Left e -> (ioError . userError . show) e
    Right (opts, args) ->
      case getCommand args of
        Left e -> (ioError . userError . show) e
        Right (cmd, rest) ->
          let config = getConfiguration opts in do
          result <- runHodorCommand config (cmd rest)
          case result of
            Left e -> (ioError . userError) e
            Right _ -> return ()
