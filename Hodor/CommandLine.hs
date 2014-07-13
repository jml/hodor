module Hodor.CommandLine where

import Data.List (intercalate)
import qualified Data.Map as M
import Data.Maybe ( fromMaybe )
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


-- XXX: It's a bit crappy having these defaults specified twice over
tFile,dFile :: Maybe String -> Flag
tFile = TodoFile . fromMaybe defaultTodoFile
dFile = DoneFile  . fromMaybe defaultDoneFile


hodorOpts :: [String] -> Either IOError ([Flag], [String])
hodorOpts argv =
  case getOpt Permute options argv of
    (o,n,[]  ) -> Right (o,n)
    (_,_,errs) -> Left (userError (concat errs ++ usageInfo header options))
  where header = "Usage: hodor [OPTION...] "


usageError :: [String] -> IOError
usageError errs = userError (concat errs ++ usageInfo header options)
                  where header = "Usage: hodor [OPTION...] "


-- XXX: Is there a better way of doing this?
-- XXX: Try out lenses
getConfiguration :: [Flag] -> Config
getConfiguration ((TodoFile path):xs) =
  let config = (getConfiguration xs)
  in config { todoFilePath = path }
getConfiguration ((DoneFile path):xs) =
  let config = (getConfiguration xs)
  in config { doneFilePath = path }
getConfiguration [] = defaultConfig


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
  putStr $ unlines $ map formatTodo $ enumerate $ items
  putStrLn "--"
  putStrLn $ printf "%s: %d of %d items shown" appName count count
  where formatTodo (i, t) = printf "%02d %s" i (unparse t)


cmdAdd :: HodorCommand
cmdAdd config args = do
  let item = intercalate " " args
      todoFile = todoFilePath config
  appendFile todoFile $ item ++ "\n"
  todos <- readTodoFileEx todoFile
  let count = length $ todoFileItems $ todos
  putStrLn $ printf "%02d %s" count item
  putStrLn $ printf "%s: %d added." appName count


-- XXX: Add with date
-- XXX: archive
-- XXX: Mark as done
-- XXX: Mark as undone
-- XXX: Filter when listing
-- XXX: External config file (yaml?)
-- XXX: Look into better idioms for errors
--      (Exceptions, ErrorT, either-as-monad)?
-- XXX: Look into better idioms for config
-- XXX: Try to get the commands out of IO

commands :: M.Map String HodorCommand
commands = M.fromList [
  ("list", cmdList),
  ("ls",   cmdList),
  ("add",  cmdAdd)
  ]


getHodorCommand :: Config -> [String] -> IO ()
getHodorCommand config [] = cmdList config []
getHodorCommand config (cmd:args) =
  case M.lookup cmd commands of
    Just command -> command config args
    Nothing -> ioError $ usageError (["No such command: ", cmd, "\n"])



main :: IO ()
main = do
  argv <- getArgs
  (opts, args) <- case (hodorOpts argv) of
    Left e -> ioError e
    Right result -> return result
  getHodorCommand (getConfiguration opts) args
