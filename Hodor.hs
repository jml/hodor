module Hodor where

import Data.Either
import Data.Maybe
import Data.Time
-- Is this the modern way to import it?
import Text.ParserCombinators.Parsec


type Date = (String, String, String)

data Project = Project { projectName :: String } deriving (Show, Eq)
data Context = Context String deriving (Show, Eq)


data TodoItem = TodoItem {
  dateCompleted :: Maybe Day,
  priority :: Maybe Char,
  dateCreated :: Maybe Day,
  projects :: [Project],
  contexts :: [Context]
} deriving (Show, Eq)


defaultTodoItem = TodoItem { dateCompleted = Nothing,
                             priority = Nothing,
                             dateCreated = Nothing,
                             projects = [],
                             contexts = [] }

todoTxtFile :: Parser [TodoItem]
todoTxtFile = endBy line eol

eol :: Parser Char
eol = char '\n'

line :: Parser TodoItem
line = do
  completed <- optionMaybe completion
  p <- optionMaybe priorityField
  created <- optionMaybe date
  words <- sepBy word (char ' ')
  let (ps, cs) = partitionEithers $ catMaybes words
  return (TodoItem completed p created ps cs)


completion = char 'x' >> char ' ' >> date

-- XXX: This is pretty hideous, I wonder if there's a better way.
-- Suggestion: a rest-of-line: Parser [Either Project Context]
word :: Parser (Maybe (Either Project Context))
word =     (try project >>= return . Just . Left)
       <|> (try context >>= return . Just . Right)
       <|> (bareword >> return Nothing)

project = do
  char '+'
  p <- bareword
  return $ Project p

context = do
  char '@'
  c <- bareword
  return $ Context c

bareword = many (noneOf " \n")


priorityField =
  do
    char '('
    p <- letter
    char ')'
    char ' '
    return p


date = do
  year <- count 4 digit
  char '-'
  month <- count 2 digit
  char '-'
  day <- count 2 digit
  char ' '
  return $ fromGregorian (read year) (read month) (read day)


-- XXX: Possibly replace (parseTodoText todoSample) in main by inlining its
-- definition and supplying `parse` with the filename, so that it comes out as
--   case parse todoTxtFile filename file_contents of -> ...

-- XXX: Don't just want to parse metadata, but also need to associate with
-- line number and with full text.  something like:
--   (i, line, parsed(line)) for i, line in enumerate(lines)
parseTodoTxt :: String -> Either ParseError [TodoItem]
parseTodoTxt input = parse todoTxtFile "(unknown)" input
