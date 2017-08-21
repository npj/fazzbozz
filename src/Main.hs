import Options.Applicative

import Fazzbozz
import CmdOptions

main = execParser opts >>= fmap printFazzbozz fillDefaultPatterns

defaultPatterns = [
    Pattern 3 "fazz",
    Pattern 5 "bozz"
  ]

fillDefaultPatterns :: CmdOptions -> CmdOptions
fillDefaultPatterns (CmdOptions n []) = CmdOptions n defaultPatterns
fillDefaultPatterns (CmdOptions n p) = CmdOptions n p

printFazzbozz :: CmdOptions -> IO ()
printFazzbozz (CmdOptions n patterns) = do
    mapM_ (putStrLn . fazzbozz (map match patterns)) [1..n]
  where match (Pattern count label) = simpleMatch count label
