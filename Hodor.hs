module Hodor where

import Control.Monad (liftM)
import Data.Either (partitionEithers)
import Data.Maybe (catMaybes)
import Data.Time (Day, fromGregorian)
import Text.ParserCombinators.Parsec


data Project = Project { projectName :: String } deriving (Show, Eq)
data Context = Context String deriving (Show, Eq)


data TodoItem = TodoItem {
  dateCompleted :: Maybe Day,
  priority :: Maybe Char,
  dateCreated :: Maybe Day,
  projects :: [Project],
  contexts :: [Context],
  description :: String
} deriving (Show, Eq)

defaultTodoItem = TodoItem { dateCompleted = Nothing,
                             priority = Nothing,
                             dateCreated = Nothing,
                             projects = [],
                             contexts = [],
                             description = "" }


-- XXX: Don't just want to parse metadata, but also need to associate with
-- line number and with full text.  something like:
--   (i, line, parsed(line)) for i, line in enumerate(lines)
parseTodoTxt :: String -> Either ParseError [TodoItem]
parseTodoTxt = parse todoTxtFile "(unknown)"


todoTxtFile :: Parser [TodoItem]
todoTxtFile = endBy todoTxtLine p_eol

p_eol :: Parser Char
p_eol = char '\n'

todoTxtLine :: Parser TodoItem
todoTxtLine = do
  all <- getInput
  completed <- optionMaybe p_completion
  p <- optionMaybe p_priority
  created <- optionMaybe p_date
  (ps, cs) <- p_projectsAndContexts
  return (TodoItem completed p created ps cs all)


p_completion = char 'x' >> char ' ' >> p_date

p_priority =
  do
    char '('
    p <- letter
    char ')'
    char ' '
    return p

p_date = do
  year <- count 4 digit
  char '-'
  month <- count 2 digit
  char '-'
  day <- count 2 digit
  char ' '
  return $ fromGregorian (read year) (read month) (read day)

p_projectsAndContexts =
  liftM (partitionEithers . catMaybes) (sepBy p_word (char ' '))


p_word :: Parser (Maybe (Either Project Context))
p_word =     liftM (Just . Left) (try p_project)
         <|> liftM (Just . Right) (try p_context)
         <|> (p_bareword >> return Nothing)

p_project = do
  char '+'
  p <- p_bareword
  return $ Project p

p_context = do
  char '@'
  c <- p_bareword
  return $ Context c

p_bareword = many (noneOf " \n")
