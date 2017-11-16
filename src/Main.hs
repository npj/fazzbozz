import Data.Maybe
import Options.Applicative
import Text.Read

import CmdOptions
import Fazzbozz

main = execParser opts >>= printFazzbozz

printFazzbozz :: CmdOptions -> IO ()
printFazzbozz (CmdOptions n matchSpecs) =
    mapM_ (putStrLn . fazzbozz matches) [1..n]
    where
      matches = map toMatch matchSpecs

toMatch :: MatchSpecifier -> Match
toMatch (ModuloMatch i s) = matchModulo i s
toMatch (FibonacciMatch s) = matchFibonacci s
