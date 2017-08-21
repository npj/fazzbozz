import Options.Applicative

import Fazzbozz
import CmdOptions

main = execParser opts >>= printFazzbozz

printFazzbozz :: CmdOptions -> IO ()
printFazzbozz (CmdOptions n patterns) = do
    mapM_ (putStrLn . fazzbozz (map match patterns)) [1..n]
  where match (Pattern count label) = simpleMatch count label
