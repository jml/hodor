module Hodor.CommandLine where

import Data.List ( intercalate )
import Data.Maybe ( fromMaybe )
import System.Console.GetOpt
import System.Environment (getArgs)

import Hodor (
  parseTodoFile
  , TodoFile
  , todoFileItems
  , unparse
  )
import Hodor.File (expandUser)


data Config = Config {
  todoFilePath :: FilePath,
  doneFilePath :: FilePath
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
                         doneFilePath = defaultDoneFile }


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


failIfLeft :: Either IOError b -> IO b
failIfLeft (Left e) = ioError e
failIfLeft (Right result) = return result


parseFile :: String -> IO (Either IOError TodoFile)
parseFile filename = do
  contents <- readFile filename
  return $ case parseTodoFile filename contents of
    -- XXX: It's not obvious that this is the best way to raise the error
    Left e -> Left (userError (show e))
    Right result -> Right result


-- XXX: Is there a better way of doing this?
getConfiguration :: [Flag] -> IO Config
getConfiguration ((TodoFile path):xs) = do
  config <- (getConfiguration xs)
  fullPath <- expandUser path
  return $ config { todoFilePath = fullPath }
getConfiguration ((DoneFile path):xs) = do
  config <- (getConfiguration xs)
  fullPath <- expandUser path
  return $ config { doneFilePath = fullPath }
getConfiguration [] = return defaultConfig



main :: IO ()
main = do
  argv <- getArgs
  (opts, args) <- failIfLeft (hodorOpts argv)
  config <- getConfiguration opts
  result <- parseFile (todoFilePath config)
  todoFile <- failIfLeft result
  putStrLn $ intercalate "\n" $ map unparse (todoFileItems todoFile)
