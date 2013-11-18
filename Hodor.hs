module Hodor (
  contexts,
  dateCreated,
  dateCompleted,
  description,
  groupByProjects,
  items,
  priority,
  projects,
  defaultTodoItem,
  todoTxtFile,
  todoTxtLine,
  Project (Project),
  Context (Context),
  Priority,
  TodoItem,
  TodoFile,
  parseTodoFile
  ) where

import Control.Arrow (first)
import Control.Monad (liftM)
import Data.Either (partitionEithers)
import Data.Function (on)
import Data.Functor ((<$>))
import Data.List (groupBy, sort)
import Data.Maybe (catMaybes)
import Data.Time (Day, fromGregorian)
import Text.ParserCombinators.Parsec


type Priority = Char

data Project = Project String deriving (Eq, Ord)

instance Show Project where
  show (Project p) = '+':p

data Context = Context String deriving (Eq, Ord)

instance Show Context where
  show (Context c) = '@':c


data TodoItem = TodoItem {
  dateCompleted :: Maybe Day,
  priority :: Maybe Priority,
  dateCreated :: Maybe Day,
  projects :: [Project],
  contexts :: [Context],
  description :: String
} deriving (Show, Eq, Ord)


defaultTodoItem = TodoItem { dateCompleted = Nothing,
                             priority = Nothing,
                             dateCreated = Nothing,
                             projects = [],
                             contexts = [],
                             description = "" }


data TodoFile = TodoFile {
  filename :: String,
  items :: [TodoItem]
} deriving (Show, Eq, Ord)



decorate :: (a -> [b]) -> a -> [(b, a)]
decorate f x = zip (f x) (repeat x)

groupByKeys :: (Ord a, Ord b) => (a -> [b]) -> [a] -> [(b, [a])]
groupByKeys f = map (first head . unzip) . groupBy (on (==) fst) . sort . concatMap (decorate f)

groupByKey :: (Ord a, Ord b) => (a -> b) -> [a] -> [(b, [a])]
groupByKey f = groupByKeys ((: []) . f)


groupByProjects :: [TodoItem] -> [(Project, [TodoItem])]
groupByProjects = groupByKeys projects


parseTodoFile :: String -> String -> Either ParseError TodoFile
parseTodoFile filename contents =
  fmap (TodoFile filename) (parse todoTxtFile filename contents)


parseTodoTxt :: String -> String -> Either ParseError [TodoItem]
parseTodoTxt = parse todoTxtFile


todoTxtFile :: Parser [TodoItem]
todoTxtFile = endBy todoTxtLine p_eol

p_eol :: Parser Char
p_eol = char '\n'

todoTxtLine :: Parser TodoItem
todoTxtLine = do
  all <- takeWhile (/= '\n') <$> getInput
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
