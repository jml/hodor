import Data.List (intercalate)
import System.IO

import Hodor (parseTodoFile, groupByProjects, items, description, Project, TodoItem, TodoFile)


renderProjectAndItems :: (Project, [TodoItem]) -> String
renderProjectAndItems (p, ts) =
  show p ++ "\n" ++ intercalate "\n" (map (("  "++) . description) ts) ++ "\n"


projectReview :: TodoFile -> String
projectReview = concat . map renderProjectAndItems . groupByProjects . items


parseFile filename = do
  todo_h <- openFile filename ReadMode
  contents <- hGetContents todo_h
  putStrLn $ case parseTodoFile filename contents of
    Left error -> show error
    Right todoFile -> projectReview todoFile
  hClose todo_h


-- XXX: How do you do command-line arguments?
-- System.Console.GetOpt
main :: IO ()
main = parseFile "/home/jml/.todo/todo.txt"
