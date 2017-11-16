import Data.Maybe
import Options.Applicative
import Text.Read

import CmdOptions
import Fazzbozz

main = execParser opts >>= printFazzbozz

printFazzbozz :: CmdOptions -> IO ()
printFazzbozz (CmdOptions n matchSpecs) =
    mapM_ (putStrLn . fazzbozz matchLPs) [1..n]
    where
      matchLPs = map toLabeledPredicate matchSpecs
      toLabeledPredicate (label, spec) = (label, toPredicate spec)

toPredicate :: MatchPredicateSpecifier -> MatchPredicate
toPredicate (ModuloPredicate i) = isModulo i
toPredicate FibonacciPredicate = isFibonacci
