module Hodor.CommandLine where

import Data.Maybe (fromMaybe)
import Options.Applicative

import Hodor.Commands
import Hodor.Config
import Hodor.Types (Priority, makePriority)


data HodorOptions = HodorOptions {
  optTodoFile :: Maybe String,
  optDoneFile :: Maybe String,
  optConfigFile :: Maybe String,
  optCommand :: Maybe Command
  } deriving (Show)


hodorInfo :: ParserInfo HodorOptions
hodorInfo = info (helper <*> globalOptions) (fullDesc <> header "hodor - a simple-minded todo list")


globalOptions :: Parser HodorOptions
globalOptions = HodorOptions
      <$> optional (strOption (long "todo-file" <> metavar "FILE" <> help "location of todo file"))
      <*> optional (strOption (long "done-file" <> metavar "FILE" <> help "location of done file"))
      <*> optional (strOption (long "config-file" <> metavar "FILE" <> help "location of config file"))
      <*> optional (subcommands hodorCommands)


-- XXX: Move this to a utility section, away from the hodor-specific
-- configuration.
subcommands :: [(String, ParserInfo a)] -> Parser a
subcommands cmds = subparser $ mconcat $ map (uncurry command) cmds


-- XXX: Find out if there's a better way to do this.
invertAssoc :: [(a, [b])] -> [(b, a)]
invertAssoc =
  concatMap reverseFlatten
  where reverseFlatten (x, ys) = [(y, x) | y <- ys]


hodorCommands :: [(String, ParserInfo Command)]
hodorCommands = invertAssoc [
  (add,     ["add",      "a"]),
  (append,  ["append",   "app"]),
  (archive, ["archive"]),
  (depri,   ["depri",    "dp"]),
  (doCmd,   ["do"]),
  (list,    ["list",     "ls"]),
  (lsc,     ["listcon",  "lsc"]),
  (lsp,     ["listpri",  "lsp"]),
  (lsprj,   ["listproj", "lsprj"]),
  (prepend, ["prepend",  "prep"]),
  (pri,     ["pri",      "p"]),
  (undo,    ["undo"])
  ]
  where
    add     = info addOptions (progDesc "add todo item")
    archive = info (pure ArchiveCommand) (progDesc "archive done items")
    append  = info appendOptions (progDesc "append text to an item")
    depri   = info depriOptions (progDesc "de-prioritize an item")
    doCmd   = info doOptions (progDesc "mark item as done")
    list    = info listOptions (progDesc "list todo items")
    lsc     = info (pure ListContextCommand) (progDesc "list contexts")
    lsp     = info (pure ListPriorityCommand) (progDesc "list items of high priority")
    lsprj   = info (pure ListProjectCommand) (progDesc "list projects")
    prepend = info prependOptions (progDesc "prepend text to an item")
    pri     = info priOptions (progDesc "prioritize an item")
    undo    = info undoOptions (progDesc "mark item as not done")

    addOptions     = AddCommand <$> wordArgs
    appendOptions  = AppendCommand <$> itemArg <*> wordArgs
    depriOptions   = DeprioritizeCommand <$> itemArg
    doOptions      = DoCommand <$> itemArgs
    listOptions    = ListCommand <$> many (argument str (metavar "FILTERS..."))
    prependOptions = PrependCommand <$> itemArg <*> wordArgs
    priOptions     = PrioritizeCommand <$> itemArg <*> priArg
    undoOptions    = UndoCommand <$> itemArgs

    itemArg  = argument auto (metavar "ITEM")
    itemArgs = some itemArg
    priArg   = argument (str >>= parsePriority) (metavar "PRIORITY")
    wordArgs = many (argument str (metavar "WORDS..."))


parsePriority :: Monad m => String -> m Priority
parsePriority [c] =
  case makePriority c of
    Just p  -> return p
    Nothing -> parsePriority []
parsePriority x      = fail $ "Invalid priority: " ++ x


defaultConfigFile :: FilePath
defaultConfigFile = "~/.hodor/config.yaml"


updateConfiguration :: Config -> HodorOptions -> Config
updateConfiguration config opts =
  config {
    todoFilePath = fromMaybe (todoFilePath config) (optTodoFile opts),
    doneFilePath = fromMaybe (doneFilePath config) (optDoneFile opts)
    }


getHodorConfiguration :: HodorOptions -> IO Config
getHodorConfiguration opts = do
  let configFilePath = fromMaybe defaultConfigFile (optConfigFile opts)
  baseConfig <- loadConfigFile configFilePath
  return $ updateConfiguration baseConfig opts


execParserWithArgs :: ParserInfo a -> [String] -> IO a
execParserWithArgs opts args =
  handleParseResult $ execParserPure (prefs idm) opts args


getCommand :: Config -> HodorOptions -> IO Command
getCommand cfg opts =
  case optCommand opts of
    Just cmd -> return cmd
    Nothing  ->
      getCommandByName $ case defaultCommand cfg of
        Just dflt -> [dflt]
        Nothing   -> []
  where
    -- XXX: A bit of a hack to a) extract command by name; b) display some
    -- kind of error on result.
    getCommandByName =
      execParserWithArgs (info (subcommands hodorCommands) idm)


main :: IO ()
main = do
  opteroos <- execParser hodorInfo
  config <- getHodorConfiguration opteroos
  cmd <- getCommand config opteroos
  result <-runHodorM (dispatchCommand cmd) config
  case result of
    Left e -> ioError (userError e)
    Right _ -> return ()
