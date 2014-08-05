{-# LANGUAGE OverloadedStrings #-}

module Hodor.Config (
  Config,
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
  defaultCommand :: Maybe String
} deriving (Show)


instance FromJSON Config where
  parseJSON (Object v) = do
    todoFile <- v .:? "todo-file" .!= defaultTodoFile
    doneFile <- v .:? "done-file" .!= defaultDoneFile
    dateOnAddSetting <- v .:? "date-on-add" .!= True
    defaultCommandSetting <- v .:? "default-command"
    return $ Config todoFile doneFile dateOnAddSetting defaultCommandSetting


defaultTodoFile, defaultDoneFile :: FilePath
defaultTodoFile = "todo.txt"
defaultDoneFile = "done.txt"


defaultConfig :: Config
defaultConfig = Config { todoFilePath = defaultTodoFile,
                         doneFilePath = defaultDoneFile,
                         defaultCommand = Just "list",
                         dateOnAdd = True }


loadConfigFile :: FilePath -> IO Config
loadConfigFile path = do
  expandedPath <- expandUser path
  config <- decodeFile expandedPath
  return $ case config of
    Just c -> c
    Nothing -> defaultConfig
