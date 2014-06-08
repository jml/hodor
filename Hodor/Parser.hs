module Hodor.Parser where

import Text.ParserCombinators.Parsec

import Control.Monad (liftM)
import Data.Either (partitionEithers)
import Data.Functor ((<$>))

import Data.Maybe (catMaybes)
import Data.Time (Day, fromGregorian)

import Hodor.Types (
  TodoFile(TodoFile),
  TodoItem(TodoItem),
  Project(Project),
  Context(Context),
  )


parseTodoFile :: FilePath -> String -> Either ParseError TodoFile
parseTodoFile filename contents =
  fmap (TodoFile filename) (parse todoTxtFile filename contents)


parseTodoTxt :: FilePath -> String -> Either ParseError [TodoItem]
parseTodoTxt = parse todoTxtFile


todoTxtFile :: Parser [TodoItem]
todoTxtFile = endBy todoTxtLine p_eol

p_eol :: Parser Char
p_eol = char '\n'

todoTxtLine :: Parser TodoItem
todoTxtLine = do
  wholeLine <- takeWhile (/= '\n') <$> getInput
  completed <- optionMaybe p_completion
  p <- optionMaybe p_priority
  created <- optionMaybe p_date
  (ps, cs) <- p_projectsAndContexts
  return (TodoItem completed p created ps cs wholeLine)


p_completion :: Parser Day
p_completion = char 'x' >> char ' ' >> p_date

p_priority :: Parser Char
p_priority =
  do
    char '('
    p <- letter
    char ')'
    char ' '
    return p

p_date :: Parser Day
p_date = do
  year <- count 4 digit
  char '-'
  month <- count 2 digit
  char '-'
  day <- count 2 digit
  char ' '
  return $ fromGregorian (read year) (read month) (read day)

p_projectsAndContexts :: Parser ([Project], [Context])
p_projectsAndContexts =
  liftM (partitionEithers . catMaybes) (sepBy p_word (char ' '))


p_word :: Parser (Maybe (Either Project Context))
p_word =     liftM (Just . Left) (try p_project)
         <|> liftM (Just . Right) (try p_context)
         <|> (p_bareword >> return Nothing)

p_project :: Parser Project
p_project = do
  char '+'
  p <- p_bareword
  return $ Project p

p_context :: Parser Context
p_context = do
  char '@'
  c <- p_bareword
  return $ Context c

p_bareword :: Parser String
p_bareword = many (noneOf " \n")
