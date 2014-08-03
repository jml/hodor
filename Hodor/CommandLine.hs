module Hodor.CommandLine where

import Control.Monad.Error (Error, ErrorT, MonadError, runErrorT, strMsg, throwError)
import Control.Monad.Trans (liftIO)
import qualified Data.Map as M
import Data.Maybe ( fromMaybe )
import System.Console.GetOpt
import System.Environment (getArgs)


import Hodor.Commands (
  HodorCommand,
  cmdAdd,
  cmdAppend,
  cmdArchive,
  cmdDeprioritize,
  cmdList,
  cmdListContexts,
  cmdListPriority,
  cmdListProjects,
  cmdMarkAsDone,
  cmdPrepend,
  cmdPrioritize,
  cmdUndo,
  runHodorCommand
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
  case getOpt RequireOrder options argv of
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
  , ("lsp", cmdListPriority)
  , ("add",  cmdAdd)
  , ("do", cmdMarkAsDone)
  , ("archive", cmdArchive)
  , ("lsc", cmdListContexts)
  , ("listcon", cmdListContexts)
  , ("lsprj", cmdListProjects)
  , ("undo", cmdUndo)
  , ("pri", cmdPrioritize)
  , ("p", cmdPrioritize)
  , ("depri", cmdDeprioritize)
  , ("dp", cmdDeprioritize)
  , ("append", cmdAppend)
  , ("app", cmdAppend)
  , ("prepend", cmdPrepend)
  , ("pre", cmdPrepend)
  ]


getCommand :: (Error e, MonadError e m) => Maybe String -> [String] -> m (HodorCommand, [String])
getCommand _ (name:rest) =
  case M.lookup name commands of
    Just command -> return (command, rest)
    Nothing -> throwError $ usageError ["No such command: ", name, "\n"]
getCommand (Just cmd) [] = getCommand Nothing [cmd]
getCommand Nothing    _  = throwError $ usageError ["Must specify a command\n"]


-- XXX: There *must* be some other way to do this.
eitherToError :: (Error e, MonadError e m) => Either e a -> m a
eitherToError (Right x) = return x
eitherToError (Left x)  = throwError x


main :: IO ()
main = do
  argv <- getArgs
  result <- runErrorT $ do
    (opts, args) <- hodorOpts argv
    cfg <- return (getConfiguration opts)
    (cmd, rest) <- getCommand (defaultCommand cfg) args
    r <- liftIO $ runHodorCommand cmd cfg rest
    eitherToError r
  case result of
    Left e -> (ioError . userError) e
    Right _ -> return ()
