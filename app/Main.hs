import Data.Maybe
import Options.Applicative
import Text.Read

import CmdOptions
import Fazzbozz

main = execParser opts >>= printFazzbozz

printFazzbozz :: (Integral n, Show n) => CmdOptions n -> IO ()
printFazzbozz (CmdOptions n matchSpecs) =
  mapM_ putStrLn $ statefulScan sfazzbozz matcher [1..n]
    where matcher = matchTogether $ map (uncurry bindMatch) matchSpecs

bindMatch :: (Integral n) => String -> MatchPredicateSpecifier n -> BoundMatcher n
bindMatch label spec = bindLabel label $ makeMatcher spec

makeMatcher :: (Integral n) => MatchPredicateSpecifier n -> ChainingMatcher n
makeMatcher (ModuloPredicate i) = moduloMatcher i
makeMatcher FibonacciPredicate = fibonacciMatcher
makeMatcher HappyPredicate = happyMatcher
