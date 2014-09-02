module Hodor.File (
  expandUser,
  readTodoFile,
  ) where

import Control.Applicative
import System.Directory (getHomeDirectory)
import System.Posix.User (getUserEntryForName, homeDirectory)

import Hodor.Parser (ParseError, parseTodoFile)
import Hodor.Types (TodoFile)


readTodoFile :: String -> IO (Either ParseError TodoFile)
readTodoFile filename = parseTodoFile filename <$> readFile filename


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
