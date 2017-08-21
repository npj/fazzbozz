module TestCmdOptions (optionsTests) where

import Test.HUnit
import Options.Applicative

import CmdOptions

parseCmdLine :: [String] -> Maybe CmdOptions
parseCmdLine = getParseResult <$> execParserPure defaultPrefs opts

optionsTests = test [
    "no args" ~: parseCmdLine [] ~?= Just CmdOptions {
      number = 20,
      patterns = [Pattern 3 "fazz", Pattern 5 "bozz"]
    },
    "-n" ~: parseCmdLine ["-n", "10"] ~?= Just CmdOptions {
      number = 10,
      patterns = [Pattern 3 "fazz", Pattern 5 "bozz"]
    },
    "-p" ~: parseCmdLine ["-p", "3:fizz", "-p", "5:buzz", "-p", "2:foo"] ~?= Just CmdOptions {
      number = 20,
      patterns = [Pattern 3 "fizz", Pattern 5 "buzz", Pattern 2 "foo"]
    }
  ]
