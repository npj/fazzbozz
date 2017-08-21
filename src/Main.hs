import Options.Applicative

import Fazzbozz
import CmdOptions

main = printFazzbozz =<< execParser opts

defaultPatterns = [
    Pattern 3 "fazz",
    Pattern 5 "bozz"
  ]

printFazzbozz :: CmdOptions -> IO ()
printFazzbozz (CmdOptions n []) = printFazzbozz' n defaultPatterns
printFazzbozz (CmdOptions n patterns) = printFazzbozz' n patterns

printFazzbozz' :: Int -> [Pattern] -> IO ()
printFazzbozz' n patterns = do
    mapM (putStrLn . fazzbozz (map match patterns)) [1..n]
    return ()
  where match (Pattern count label) = simpleMatch count label
