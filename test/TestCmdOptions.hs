module TestCmdOptions (optionsTests) where

import Test.HUnit
import Options.Applicative

import CmdOptions

parseCmdLine :: [String] -> Maybe CmdOptions
parseCmdLine args =
  case result of
      Success opts -> Just opts
      otherwise -> Nothing
    where result = execParserPure defaultPrefs opts args

optionsTests = test [
    "no args" ~: parseCmdLine [] ~?= Just CmdOptions {
      number = 20,
      patterns = []
    },
    "-n" ~: parseCmdLine ["-n", "10"] ~?= Just CmdOptions {
      number = 10,
      patterns = []
    },
    "-p" ~: parseCmdLine ["-p", "3:fazz", "-p", "5:bozz"] ~?= Just CmdOptions {
      number = 20,
      patterns = [Pattern 3 "fazz", Pattern 5 "bozz"]
    }
  ]
