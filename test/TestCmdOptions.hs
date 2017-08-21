module TestCmdOptions (optionsTests) where

import Test.HUnit
import Options.Applicative

import CmdOptions

parseCmdLine :: [String] -> Maybe CmdOptions
parseCmdLine = getParseResult <$> execParserPure defaultPrefs opts

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
