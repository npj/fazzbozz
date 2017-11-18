import Data.Maybe
import Options.Applicative
import Text.Read

import CmdOptions
import Fazzbozz

main = execParser opts >>= printFazzbozz

printFazzbozz :: CmdOptions -> IO ()
printFazzbozz (CmdOptions n matchSpecs) =
    mapM_ (putStrLn . mfazzbozz matchSpecs) [1..n]

instance Matchable MatchPredicateSpecifier where
  match (ModuloPredicate i) = isModulo i
  match FibonacciPredicate = isFibonacci
