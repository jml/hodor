module Hodor.Config (
  Config,
  dateOnAdd,
  defaultConfig,
  defaultDoneFile,
  defaultTodoFile,
  doneFilePath,
  todoFilePath
  ) where


data Config = Config {
  todoFilePath :: FilePath,
  doneFilePath :: FilePath,
  dateOnAdd :: Bool
} deriving (Show)


defaultTodoFile, defaultDoneFile :: FilePath
defaultTodoFile = "todo.txt"
defaultDoneFile = "done.txt"

defaultConfig :: Config
defaultConfig = Config { todoFilePath = defaultTodoFile,
                         doneFilePath = defaultDoneFile,
                         dateOnAdd = True }


-- XXX: External config file (yaml?)
-- XXX: Look into better idioms for config
--      - Reader monad?
--      - lenses?
