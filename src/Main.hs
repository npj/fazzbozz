import Options.Applicative

import Fazzbozz
import CmdOptions

main = printFazzbozz =<< execParser opts

printFazzbozz :: CmdOptions -> IO ()
printFazzbozz (CmdOptions n) = do
  mapM (putStrLn . fazzbozz) [1..n]
  return ()
