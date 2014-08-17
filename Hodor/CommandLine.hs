{-# LANGUAGE FlexibleContexts #-}

module Hodor.CommandLine where

import Data.Maybe (fromMaybe)
import Options.Applicative

import Hodor.Commands
import Hodor.Config


data Options = Options {
  optTodoFile :: Maybe String,
  optDoneFile :: Maybe String,
  optConfigFile :: Maybe String,
  optCommand :: Command
  } deriving (Show)


globalOptions :: Parser Options
globalOptions = Options
      <$> optional (strOption (long "todo-file" <> metavar "FILE" <> help "location of todo file"))
      <*> optional (strOption (long "done-file" <> metavar "FILE" <> help "location of done file"))
      <*> optional (strOption (long "config-file" <> metavar "FILE" <> help "location of config file"))
      <*> subparser (
        command "list" (info listOptions (progDesc "list todo items"))
        <> command "add" (info addOptions (progDesc "add todo item"))
        <> command "do" (info doOptions (progDesc "mark item as done"))
        <> command "undo" (info undoOptions (progDesc "mark item as not done"))
        <> command "archive" (info (pure ArchiveCommand) (progDesc "archive done items"))
        <> command "lsc" (info (pure ListContextCommand) (progDesc "list contexts"))
        <> command "lsp" (info (pure ListPriorityCommand) (progDesc "list items of high priority"))
        <> command "lsprj" (info (pure ListProjectCommand) (progDesc "list projects"))
        <> command "pri" (info priOptions (progDesc "prioritize an item"))
        <> command "depri" (info depriOptions (progDesc "de-prioritize an item"))
        <> command "app" (info appendOptions (progDesc "append text to an item"))
        <> command "pre" (info prependOptions (progDesc "prepend text to an item")))

-- XXX: Add aliases
-- XXX: Maybe move stuff into 'where' of globalOptions
-- XXX: Specific reader for priority (i.e. delete Commands.getPriority)

itemOption :: Parser Int
itemOption = argument auto (metavar "ITEM")

itemsOption :: Parser [Int]
itemsOption = some itemOption

wordsOption :: Parser [String]
wordsOption = some (argument str (metavar "WORDS..."))

addOptions :: Parser Command
addOptions = AddCommand <$> wordsOption

doOptions = DoCommand <$> itemsOption

undoOptions = UndoCommand <$> itemsOption

depriOptions = DeprioritizeCommand <$> itemOption

listOptions :: Parser Command
listOptions = ListCommand <$> many (argument str (metavar "FILTERS..."))

priOptions :: Parser Command
priOptions = PrioritizeCommand <$> itemOption <*> (argument str (metavar "PRIORITY"))

appendOptions :: Parser Command
appendOptions = AppendCommand <$> itemOption <*> wordsOption

prependOptions :: Parser Command
prependOptions = PrependCommand <$> itemOption <*> wordsOption


-- Something that turns options into config
-- Significantly change how we run the commands

defaultConfigFile :: FilePath
defaultConfigFile = "~/.hodor/config.yaml"


updateConfiguration :: Config -> Options -> Config
updateConfiguration config opts =
  config {
    todoFilePath = fromMaybe (todoFilePath config) (optTodoFile opts),
    doneFilePath = fromMaybe (doneFilePath config) (optDoneFile opts)
    }


getHodorConfiguration :: Options -> IO Config
getHodorConfiguration opts = do
  let configFilePath = fromMaybe defaultConfigFile (optConfigFile opts)
  baseConfig <- loadConfigFile configFilePath
  return $ updateConfiguration baseConfig opts


main :: IO ()
main = do
  opteroos <- execParser opts
  config <- getHodorConfiguration opteroos
  result <- runHodorM (dispatchCommand (optCommand opteroos)) config
  case result of
    Left e -> ioError (userError e)
    Right _ -> return ()
  where opts = info (helper <*> globalOptions) (
          fullDesc
          <> header "hodor - a simple-minded todo list" )
