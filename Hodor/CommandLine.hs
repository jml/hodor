module Hodor.CommandLine where

import Control.Monad.Error (Error, ErrorT, MonadError, runErrorT, strMsg, throwError)
import Control.Monad.Trans (liftIO)
import qualified Data.Map as M
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


data Flag = ConfigFile (Maybe FilePath)
          | TodoFile (Maybe FilePath)
          | DoneFile (Maybe FilePath)


options :: [OptDescr Flag]
options =
    [ Option ['t'] ["todo-file"] (OptArg TodoFile "FILE") "location of todo file"
    , Option ['d'] ["done-file"] (OptArg DoneFile "FILE") "location of done file"
    , Option ['c'] ["config-file"] (OptArg ConfigFile "FILE") "location of config file"
    ]


hodorOpts :: (Error e, MonadError e m) => [String] -> m ([Flag], [String])
hodorOpts argv =
  case getOpt RequireOrder options argv of
    (o,n,[]  ) -> return (o,n)
    (_,_,errs) -> throwError (usageError errs)


usageError :: (Error e) => [String] -> e
usageError errs = strMsg (concat errs ++ usageInfo header options)
                  where header = "Usage: hodor [OPTION...] "


defaultConfigFile :: FilePath
defaultConfigFile = "~/.hodor/config.yaml"


getConfigFilePath :: FilePath -> [Flag] -> FilePath
getConfigFilePath defaultPath flags =
  case [ path | ConfigFile path <- flags ] of
    (Just path):[] -> path
    _              -> defaultPath


updateConfiguration :: Config -> Flag -> Config
updateConfiguration config (TodoFile (Just path)) = config { todoFilePath = path }
updateConfiguration config (DoneFile (Just path)) = config { doneFilePath = path }
updateConfiguration config _                      = config


getHodorConfiguration :: [Flag] -> IO Config
getHodorConfiguration opts = do
  let configFilePath = getConfigFilePath defaultConfigFile opts
  baseConfig <- loadConfigFile configFilePath
  return $ foldl updateConfiguration baseConfig opts


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
    cfg <- liftIO $ getHodorConfiguration opts
    (cmd, rest) <- getCommand (defaultCommand cfg) args
    r <- liftIO $ runHodorCommand cmd cfg rest
    eitherToError r
  case result of
    Left e -> (ioError . userError) e
    Right _ -> return ()
