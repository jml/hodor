module Hodor.CommandLine where

import Control.Monad.Error (Error, strMsg, throwError, throwIO)
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


import Hodor (
  readTodoFile
  , TodoFile
  , todoFileItems
  , unparse
  )
import Hodor.File (expandUser)


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
defaultTodoFile = "/Users/jml/.todo/todo.txt"
defaultDoneFile = "/Users/jml/.todo/done.txt"

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
    (o,n,[]  ) -> Right (o,n)
    (_,_,errs) -> Left (usageError errs)
  where header = "Usage: hodor [OPTION...] "


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


-- TODO: Bust this apart and use the newly made ParseError to try to get this
-- whole module to use Control.Monad.Error rather than exceptions.
--
-- Will probably need a wrapper ADT to bring in opt errors & parse errors
readTodoFileEx :: FilePath -> IO TodoFile
readTodoFileEx path = do
  expanded <- expandUser path
  result <- readTodoFile expanded
  case result of
    Left e -> ioError $ userError $ show e
    Right r -> return r


type HodorCommand = Config -> [String] -> IO ()


-- XXX: Does Haskell have this already?
enumerate :: [a] -> [(Integer, a)]
enumerate = zip [1..]


appName :: String
appName = "HODOR"


-- Here we number items according to how they appear, but actually the number
-- is intrinsic to the item, and should probably be associated when parsed.
cmdList :: HodorCommand
cmdList config _ = do
  todoFile <- readTodoFileEx (todoFilePath config)
  let items = todoFileItems todoFile
      count = length items
  putStr $ unlines $ map formatTodo $ sortTodo $ enumerate $ map unparse $ items
  putStrLn "--"
  putStrLn $ printf "%s: %d of %d items shown" appName count count
  where formatTodo (i, t) = printf "%02d %s" i t
        sortTodo = sortWith snd


cmdAdd :: HodorCommand
cmdAdd config args = do
  -- XXX: This bit (add today's date if config says so) is hideous
  allArgs <- case (dateOnAdd config) of
    True -> today >>= \x -> return $ (show x):args
    False -> return args
  let item = intercalate " " allArgs
      todoFile = todoFilePath config
  appendFile todoFile $ item ++ "\n"
  todos <- readTodoFileEx todoFile
  let count = length $ todoFileItems $ todos
  putStrLn $ printf "%02d %s" count item
  putStrLn $ printf "%s: %d added." appName count


-- XXX: Make tests for this stuff, dammit (see 'get out of IO' below)
-- XXX: Make a test harness for command-line testing
-- XXX: Colorize
-- XXX: archive
-- XXX: Priority list (lsp)
-- XXX: Mark as done
-- XXX: Mark as undone
-- XXX: Filter when listing
-- XXX: External config file (yaml?)
-- XXX: Look into better idioms for errors
--      (Exceptions, ErrorT, either-as-monad)?
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

commands :: M.Map String HodorCommand
commands = M.fromList [
  ("list", cmdList),
  ("ls",   cmdList),
  ("add",  cmdAdd)
  ]


getHodorCommand :: [String] -> Either UserError ([String] -> IO (), [String])
getHodorCommand argv = do
  (opt, args) <- hodorOpts argv
  let config = (getConfiguration opt)
  case args of
    [] -> return (cmdList config, [])
    (name:rest) ->
      case M.lookup name commands of
        Just command -> return (command config, rest)
        Nothing -> throwError $ UserError (concat ["No such command: ", name, "\n"])


main :: IO ()
main = do
  argv <- getArgs
  -- XXX: I can't help but feel that if I understood monad transformers better
  -- I could make this cleaner.
  case getHodorCommand argv of
    Left e -> throwIO e
    Right (cmd, rest) -> cmd rest
