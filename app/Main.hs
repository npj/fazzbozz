import Data.Maybe
import Options.Applicative
import Text.Read

import CmdOptions
import Fazzbozz

main = execParser opts >>= printFazzbozz

printFazzbozz :: (Integral n, Show n) => CmdOptions n -> IO ()
printFazzbozz (CmdOptions n matchSpecs) =
  mapM_ putStrLn $ statefulScan sfazzbozz matchers [1..n]
    where matchers = map bindMatch matchSpecs

bindMatch :: (Integral n) => (String, MatchPredicateSpecifier n) -> BoundMatcher [n] n
bindMatch (label, (ModuloPredicate i)) = BoundMatcher label (voidState $ isModulo i) []
bindMatch (label, FibonacciPredicate) = bindFibonacci label
