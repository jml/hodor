{-# LANGUAGE OverloadedStrings #-}

module Hodor.Config (
  Config,
  colorEnabled,
  dateOnAdd,
  defaultCommand,
  defaultConfig,
  doneFilePath,
  loadConfigFile,
  todoFilePath
  ) where

import Data.Yaml

import Hodor.File (expandUser)


-- XXX: Try out lenses for this.

data Config = Config {
  todoFilePath :: FilePath,
  doneFilePath :: FilePath,
  dateOnAdd :: Bool,
  defaultCommand :: Maybe String,
  colorEnabled :: Bool
} deriving (Show)


instance FromJSON Config where
  parseJSON (Object v) = do
    todoFile <- v .:? "todo-file" .!= defaultTodoFile
    doneFile <- v .:? "done-file" .!= defaultDoneFile
    dateOnAddSetting <- v .:? "date-on-add" .!= True
    defaultCommandSetting <- v .:? "default-command"
    return $ Config todoFile doneFile dateOnAddSetting defaultCommandSetting True


defaultTodoFile, defaultDoneFile :: FilePath
defaultTodoFile = "todo.txt"
defaultDoneFile = "done.txt"


defaultConfig :: Config
defaultConfig = Config { todoFilePath = defaultTodoFile,
                         doneFilePath = defaultDoneFile,
                         defaultCommand = Just "list",
                         dateOnAdd = True,
                         colorEnabled = True }


loadConfigFile :: FilePath -> IO Config
loadConfigFile path = do
  expandedPath <- expandUser path
  config <- decodeFile expandedPath
  expandConfigPaths $ case config of
    Just c -> c
    Nothing -> defaultConfig


expandConfigPaths :: Config -> IO Config
expandConfigPaths config = do
  todoPath <- expandUser (todoFilePath config)
  donePath <- expandUser (doneFilePath config)
  return config { todoFilePath = todoPath, doneFilePath = donePath }
