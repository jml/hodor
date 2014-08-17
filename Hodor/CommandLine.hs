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
        command "add" add
        <> command "a" add
        <> command "append" append
        <> command "app" append
        <> command "archive" archive
        <> command "depri" depri
        <> command "dp" depri
        <> command "do" doCmd
        <> command "list" list
        <> command "ls" list
        <> command "listcon" lsc
        <> command "lsc" lsc
        <> command "listpri" lsp
        <> command "lsp" lsp
        <> command "listproj" lsprj
        <> command "lsprj" lsprj
        <> command "prepend" prepend
        <> command "prep" prepend
        <> command "pri" pri
        <> command "p" pri
        <> command "undo" undo
        )
      where
        add     = (info addOptions (progDesc "add todo item"))
        archive = (info (pure ArchiveCommand) (progDesc "archive done items"))
        append  = (info appendOptions (progDesc "append text to an item"))
        depri   = (info depriOptions (progDesc "de-prioritize an item"))
        doCmd   = (info doOptions (progDesc "mark item as done"))
        list    = (info listOptions (progDesc "list todo items"))
        lsc     = (info (pure ListContextCommand) (progDesc "list contexts"))
        lsp     = (info (pure ListPriorityCommand) (progDesc "list items of high priority"))
        lsprj   = (info (pure ListProjectCommand) (progDesc "list projects"))
        prepend = (info prependOptions (progDesc "prepend text to an item"))
        pri     = (info priOptions (progDesc "prioritize an item"))
        undo    = (info undoOptions (progDesc "mark item as not done"))

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
