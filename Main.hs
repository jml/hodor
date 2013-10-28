
import Hodor (parseTodoTxt)


-- XXX: Unit tests!
todoSample :: String
todoSample = unlines [
  "(B) 2013-09-27 Wipe mould off bathroom ceiling +condensation @home",
  "2013-09-21 Email John arranging time to catch up @online +some-project",
  "x 2013-10-12 2013-09-20 A done task +some-project",
  "x 2013-10-12 Something else"
  ]


-- XXX: How do you do command-line arguments?
-- System.Console.GetOpt
main :: IO ()
main = do
  case parseTodoTxt todoSample of
    Left e -> do putStrLn "Error parsing input"
                 print e
    Right r -> mapM_ print r
