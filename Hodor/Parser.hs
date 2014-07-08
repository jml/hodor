module Hodor.Parser where

import Text.ParserCombinators.Parsec

-- XXX: Make sure we can handle blank lines!

import Control.Monad (liftM)
import Data.Either (partitionEithers)
import Data.Functor ((<$>))

import Data.Maybe (catMaybes)
import Data.Time (Day, fromGregorian)

import Hodor.Types (
  TodoFile(TodoFile),
  TodoItem(TodoItem),
  Priority,
  Token(Bareword, ProjectToken, ContextToken),
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
  tokens <- p_tokens
  return (TodoItem completed p created tokens)


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


p_tokens :: Parser [Token]
p_tokens = many p_word

whitespace :: [Char]
whitespace = " \n"

p_word :: Parser Token
p_word = (try p_project)
         <|> (try p_context)
         <|> (fmap Bareword p_bareword)
         <|> (fmap Bareword p_whitespace)

p_project :: Parser Token
p_project = do
  char '+'
  p <- p_bareword
  return $ ProjectToken p

p_context :: Parser Token
p_context = do
  char '@'
  c <- p_bareword
  return $ ContextToken c

p_bareword :: Parser String
p_bareword = many1 (noneOf whitespace)

p_whitespace :: Parser String
p_whitespace = many1 (oneOf whitespace)
