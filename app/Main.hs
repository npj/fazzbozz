import Data.Maybe
import Options.Applicative
import Text.Read

import Fazzbozz
import Fazzbozz.CmdOptions

main = execParser opts >>= printFazzbozz

printFazzbozz :: CmdOptions Integer -> IO ()
printFazzbozz (CmdOptions n matchSpecs) =
  mapM_ putStrLn $ scanM sfazzbozz states [1..n]
    where
      states = map makeState' matchSpecs
      makeState' (label, pred) = Labeled (makeState pred) label
