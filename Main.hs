import Data.Either
import Data.Maybe
import Text.ParserCombinators.Parsec


type Date = (String, String, String)

data Project = Project String deriving Show
data Context = Context String deriving Show



data TodoItem = TodoItem {
  dateCompleted :: Maybe Date,
  priority :: Maybe Char,
  dateCreated :: Maybe Date,
  projects :: [Project],
  contexts :: [Context]
} deriving Show


todoSample = unlines [
  "(B) 2013-09-27 Wipe mould off bathroom ceiling +condensation @home",
  "2013-09-21 Email Will arranging time to catch up @online +leading-kg",
  "x 2013-10-12 2013-09-20 Read Acts +leading-kg",
  "x 2013-10-12 Something else"
  ]


todoTxtFile :: GenParser Char st [TodoItem]
todoTxtFile = endBy line eol

eol :: GenParser Char st Char
eol = char '\n'

line :: GenParser Char st TodoItem
line = do
  dateCompleted <- optionMaybe completion
  p <- optionMaybe priorityField
  created <- optionMaybe date
  words <- sepBy word (char ' ')
  let (ps, cs) = partitionEithers $ catMaybes words
    in return (TodoItem dateCompleted p created ps cs)


completion = char 'x' >> char ' ' >> date

word :: GenParser Char st (Maybe (Either Project Context))
word = (try project >>= return . Just . Left)
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
  return (year, month, day)


parseTodoTxt :: String -> Either ParseError [TodoItem]
parseTodoTxt input = parse todoTxtFile "(unknown)" input


main = do
  case parseTodoTxt todoSample of
    Left e -> do putStrLn "Error parsing input"
                 print e
    Right r -> mapM_ print r
