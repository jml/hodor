
import Text.ParserCombinators.Parsec

todoSample = unlines [
  "(B) 2013-09-27 Wipe mould off bathroom ceiling +condensation @home",
  "2013-09-21 Email Will arranging time to catch up @online +leading-kg",
  "2013-09-20 Read Acts +leading-kg"
  ]


todoTxtFile :: GenParser Char st [(Maybe Char, Maybe (String, String, String), String)]
todoTxtFile = endBy line eol

eol :: GenParser Char st Char
eol = char '\n'

line :: GenParser Char st (Maybe Char, Maybe (String, String, String), String)
line = do
  p <- optionMaybe priority
  created <- optionMaybe date
  rest <- many (noneOf "\n")
  return (p, created, rest)

priority =
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


parseTodoTxt :: String -> Either ParseError [(Maybe Char, Maybe (String, String, String), String)]
parseTodoTxt input = parse todoTxtFile "(unknown)" input


main = do
  case parseTodoTxt todoSample of
    Left e -> do putStrLn "Error parsing input"
                 print e
    Right r -> mapM_ print r
