import Control.Monad.Trans.Error (ErrorT(ErrorT), runErrorT, Error, strMsg)
import Data.Either (lefts)
import Data.Maybe (maybe)
import Data.List (intercalate)
import Data.Time
import System.IO
import Text.ParserCombinators.Parsec (ParseError, parse)

import Hodor (dateCompleted, parseTodoFile, groupByProjects, items, description, Project, TodoItem, TodoFile)


instance Error ParseError where
    strMsg s = head (lefts [(parse (fail s) "" [])])


renderProjectAndItems :: (Maybe Project, [TodoItem]) -> String
renderProjectAndItems (p, ts) =
    project ++ "\n" ++ items ++ "\n"
    where
      project = maybe "(no project)" show p
      items = intercalate "\n" (map (("  "++) . description) ts)


projectReview :: [TodoItem] -> String
projectReview = concat . map renderProjectAndItems . groupByProjects


parseFile :: String -> IO (Either ParseError TodoFile)
parseFile filename = do
  contents <- readFile filename
  return $ parseTodoFile filename contents


parseFiles :: [String] -> IO (Either ParseError [TodoFile])
parseFiles files = runErrorT $ sequence (map ErrorT (map parseFile files))

closedSince :: Day -> TodoItem -> Bool
closedSince day = maybe True (>= day) . dateCompleted


-- XXX: How do you do command-line arguments?
-- System.Console.GetOpt
main :: IO ()
main = do
  today <- getCurrentTime >>= return . utctDay
  result <- parseFiles ["/home/jml/.todo/todo.txt", "/home/jml/.todo/done.txt"]
  putStrLn $ case result of
    Left error -> show error
    Right todoFiles -> projectReview $ filter (closedSince $ addDays (-7) today) $ concatMap items todoFiles
