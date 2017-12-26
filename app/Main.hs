import Data.Maybe
import Options.Applicative
import Text.Read

import CmdOptions
import Fazzbozz

main = execParser opts >>= printFazzbozz

printFazzbozz :: (Integral n, Show n) => CmdOptions n -> IO ()
printFazzbozz (CmdOptions n matchSpecs) =
    mapM_ (putStrLn . fazzbozz matches) [1..n]
    where
      matches = mapSnd toMatch matchSpecs
      mapSnd f = map (\(a, b) -> (a, f b))

toMatch :: (Integral n) => MatchPredicateSpecifier n -> Match n
toMatch (ModuloPredicate i) = isModulo i
toMatch FibonacciPredicate = isFibonacci
