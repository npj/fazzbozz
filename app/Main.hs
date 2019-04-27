import Data.Maybe
import Options.Applicative
import Text.Read

import CmdOptions
import Fazzbozz

main = execParser opts >>= printFazzbozz

printFazzbozz :: CmdOptions Integer -> IO ()
printFazzbozz (CmdOptions n matchSpecs) =
  mapM_ putStrLn $ statefulScan sfazzbozz states [1..n]
    where
      states = map makeState' matchSpecs
      makeState' (label, pred) = (makeState pred, label)

makeState :: MatchPredicateSpecifier Integer -> EnclosedState
makeState (ModuloPredicate n) = enclose $ ModuloState n
makeState FibonacciPredicate = enclose defaultFibonacciState
makeState HappyPredicate = enclose defaultHappyState
