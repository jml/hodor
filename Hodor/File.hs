module Hodor.File (
  expandUser,
  readTodoFile,
  ) where

import System.Directory (getHomeDirectory)
import System.Posix.User (getUserEntryForName, homeDirectory)
import Text.ParserCombinators.Parsec

import Hodor.Parser (parseTodoFile)
import Hodor.Types (TodoFile)


-- XXX: Rename to readTodoFile
readTodoFile :: String -> IO (Either ParseError TodoFile)
readTodoFile filename = do
  contents <- readFile filename
  return $ parseTodoFile filename contents


-- XXX: Move to some utility class
expandUser :: FilePath -> IO FilePath
-- "~" => "/home/foo"
expandUser "~" = getHomeDirectory
-- "~/bar" => "/home/foo/bar"
expandUser ('~':'/':trail) = do
  home <- getHomeDirectory
  return $ home ++ "/" ++ trail
-- "~user/foo" => "/home/user/foo"
expandUser ('~':trail) =
  let (username, rest) = break (== '/') trail in
  do
    userEntry <- getUserEntryForName username
    return $ (homeDirectory userEntry) ++ "/" ++ (drop 1 rest)
-- "whatever" => "whatever"
expandUser p = return p
