{-# LANGUAGE FlexibleContexts #-}

module Hodor.CommandLine where

import Control.Monad.Error (Error, ErrorT, MonadError, runErrorT, strMsg, throwError)
import Control.Monad.Reader (runReaderT)
import qualified Data.Map as M
import Data.Maybe ( fromMaybe )
import System.Console.GetOpt
import System.Environment (getArgs)


import Hodor.Commands (
  HodorCommand,
  cmdMarkAsDone,
  cmdAdd,
  cmdList,
  cmdArchive
  )
import Hodor.Config


data Flag = TodoFile FilePath | DoneFile FilePath


options :: [OptDescr Flag]
options =
    [ Option ['t'] ["todo-file"] (OptArg tFile "FILE") "location of todo file"
    , Option ['d'] ["done-file"] (OptArg dFile "FILE") "location of done file"
    ]


-- XXX: It's a bit crappy having these defaults specified twice over
tFile,dFile :: Maybe String -> Flag
tFile = TodoFile . fromMaybe defaultTodoFile
dFile = DoneFile  . fromMaybe defaultDoneFile


hodorOpts :: (Error e, MonadError e m) => [String] -> m ([Flag], [String])
hodorOpts argv =
  case getOpt Permute options argv of
    (o,n,[]  ) -> return (o,n)
    (_,_,errs) -> throwError (usageError errs)


usageError :: (Error e) => [String] -> e
usageError errs = strMsg (concat errs ++ usageInfo header options)
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


commands :: M.Map String HodorCommand
commands = M.fromList [
  ("list", cmdList)
  , ("ls",   cmdList)
  , ("add",  cmdAdd)
  , ("do", cmdMarkAsDone)
  , ("archive", cmdArchive)
  ]


-- XXX: Make the default command configurable
defaultCommand :: HodorCommand
defaultCommand = cmdList


getCommand :: (Error e, MonadError e m) => [String] -> m (HodorCommand, [String])
getCommand (name:rest) =
  case M.lookup name commands of
    Just command -> return (command, rest)
    Nothing -> throwError $ usageError ["No such command: ", name, "\n"]
getCommand [] = return (defaultCommand, [])


main :: IO ()
main = do
  argv <- getArgs
  -- XXX: This breaks the abstraction around HodorCommand.
  result <- runErrorT $ do
    (opts, args) <- hodorOpts argv
    (cmd, rest) <- getCommand args
    let config = getConfiguration opts
    runReaderT (cmd rest) config
  case result of
    Left e -> (ioError . userError) e
    Right _ -> return ()
