import Data.Maybe
import Options.Applicative
import Text.Read

import CmdOptions
import Fazzbozz
import Matching

main = execParser opts >>= printFazzbozz
  where opts = makeOpts patternParsers defaultMatches

defaultMatches :: [Match]
defaultMatches = [
    simpleMatch 3 "fazz",
    simpleMatch 5 "bozz"
  ]

printFazzbozz :: CmdOptions -> IO ()
printFazzbozz (CmdOptions n matches) =
    mapM_ (putStrLn . fazzbozz matches) [1..n]
