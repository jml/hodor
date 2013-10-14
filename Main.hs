import Text.ParserCombinators.Parsec


type Date = (String, String, String)

type Project = (String, String)
type Context = (String, String)



data TodoItem = TodoItem {
  priority :: Maybe Char,
  dateCreated :: Maybe Date,
  projects :: [Project],
  contexts :: [Context]
} deriving Show


todoSample = unlines [
  "(B) 2013-09-27 Wipe mould off bathroom ceiling +condensation @home",
  "2013-09-21 Email Will arranging time to catch up @online +leading-kg",
  "2013-09-20 Read Acts +leading-kg"
  ]


todoTxtFile :: GenParser Char st [TodoItem]
todoTxtFile = endBy line eol

eol :: GenParser Char st Char
eol = char '\n'

line :: GenParser Char st TodoItem
line = do
  p <- optionMaybe priorityField
  created <- optionMaybe date
  words <- sepBy word (char ' ')
  return (TodoItem p created words [])

word = try project <|> try context
       <|> do
         w <- bareword
         return ("", w)

project = do
  char '+'
  p <- bareword
  return ("Project", p)

context = do
  char '@'
  c <- bareword
  return ("Context", c)

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
