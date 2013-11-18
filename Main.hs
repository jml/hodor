import Data.List (intercalate)
import System.IO

import Hodor (parseTodoFile, groupByProjects, items, description, Project, TodoItem, TodoFile)


renderProjectAndItems :: (Project, [TodoItem]) -> String
renderProjectAndItems (p, ts) =
  show p ++ "\n" ++ intercalate "\n" (map (("  "++) . description) ts) ++ "\n"


projectReview :: TodoFile -> String
projectReview = concat . map renderProjectAndItems . groupByProjects . items




parseFile filename = do
  contents <- readFile filename
  putStrLn $ case parseTodoFile filename contents of
    Left error -> show error
    Right todoFile -> projectReview todoFile


-- XXX: How do you do command-line arguments?
-- System.Console.GetOpt
main :: IO ()
main = parseFile "/home/jml/.todo/todo.txt"
