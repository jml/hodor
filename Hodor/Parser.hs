module Hodor.Parser (
  ParseError,
  parseTodoFile,
  parseTodoLine,
  parseTodoTxt,
  todoTxtFile,
  todoTxtLine
  ) where

import Control.Monad.Error (Error, strMsg)
import Data.Time (Day, fromGregorian)
import Text.Parsec hiding (ParseError)
import Text.Parsec.String (Parser)
import qualified Text.Parsec as P

import Hodor.Functional (onLeft, unleft)
import Hodor.Types (
  makeTodoFile,
  noPriority,
  TodoFile,
  TodoItem(TodoItem),
  Priority,
  unsafeMakePriority
  )


newtype ParseError = ParseError P.ParseError
                     deriving (Show)

instance Eq ParseError where
  (==) a b = show a == show b


instance Error ParseError where
  strMsg = ParseError . unleft . makeParseError
    where makeParseError s = parse (fail s) "" []


parseTodoFile :: FilePath -> String -> Either ParseError TodoFile
parseTodoFile filename contents =
  fmap (makeTodoFile filename) (parseTodoTxt filename contents)


parseTodoTxt :: FilePath -> String -> Either ParseError [TodoItem]
parseTodoTxt f = onLeft ParseError . parse todoTxtFile f


parseTodoLine :: String -> Either ParseError TodoItem
parseTodoLine = onLeft ParseError . parse todoTxtLine "(line)"


todoTxtFile :: Parser [TodoItem]
todoTxtFile = do
  todoLines <- endBy p_todoTxtLine p_eol
  return [ x | Just x <- todoLines ]


p_eol :: Parser Char
p_eol = char '\n'

p_todoTxtLine :: Parser (Maybe TodoItem)
p_todoTxtLine = try (many (oneOf " \t") >> lookAhead p_eol >> return Nothing)
                <|> (fmap Just todoTxtLine)


todoTxtLine :: Parser TodoItem
todoTxtLine = do
  completed <- optionMaybe p_completion
  p <- p_priority
  created <- optionMaybe p_date
  todo_tokens <- many (noneOf "\n")
  return $ TodoItem completed p created todo_tokens


p_completion :: Parser Day
p_completion = char 'x' >> char ' ' >> p_date

p_priority :: Parser Priority
p_priority = do
  p <- optionMaybe p_priorityChar
  case p of
    Nothing -> return noPriority
    Just c  -> return (unsafeMakePriority c)

p_priorityChar :: Parser Char
p_priorityChar =
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
