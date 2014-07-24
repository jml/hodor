module Hodor.Parser (
  ParseError(ParseError),
  parseTodoFile,
  parseTodoTxt,
  todoTxtFile,
  todoTxtLine
  ) where

import Control.Monad.Error (Error, strMsg)
import Data.Char (isSpace)
import Data.Time (Day, fromGregorian)
import Text.Parsec hiding (ParseError)
import Text.Parsec.String (Parser)
import qualified Text.Parsec as P

import Hodor.Functional (onLeft, unleft)
import Hodor.Types (
  makePriority,
  makeTodoFile,
  TodoFile,
  TodoItem(TodoItem),
  Priority,
  Token(Bareword, ProjectToken, ContextToken),
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
  p <- optionMaybe p_priority
  created <- optionMaybe p_date
  todo_tokens <- p_tokens
  return $ TodoItem completed p created todo_tokens


p_completion :: Parser Day
p_completion = char 'x' >> char ' ' >> p_date

p_priority :: Parser Priority
p_priority =
  do
    char '('
    p <- letter
    char ')'
    char ' '
    return $ makePriority p

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

p_word :: Parser Token
p_word = (try p_project)
         <|> (try p_context)
         <|> try (fmap Bareword p_bareword)
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
p_bareword = many1 (satisfy (not . isSpace))

p_whitespace :: Parser String
p_whitespace = many1 (oneOf " \t")
