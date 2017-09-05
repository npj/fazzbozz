module TestCmdOptions (optionsTests) where

import Test.HUnit
import Options.Applicative

import CmdOptions

makeCmdLineParser :: ParserInfo CmdOptions -> [String] -> Maybe CmdOptions
makeCmdLineParser opts = getParseResult <$> execParserPure defaultPrefs opts

minimalOptionsTest = 
  let parseCmdLine = makeCmdLineParser $ makeOpts [] []
  in [
    "default number" ~: number <$> parseCmdLine [] ~=? Just 20,
    "specify number" ~: number <$> parseCmdLine ["-n", "100"] ~=? Just 100,

    "default matches" ~: length . matches <$> parseCmdLine [] ~=? Just 0,
    -- parseCmdLine actually returns Nothing here, but Maybe CmdOptions
    -- isn't in Eq, so we can't ~=? it. we fmap (length . matches) just to
    -- get something in Eq
    "reject all patterns" ~: length . matches <$> parseCmdLine ["-p", "2:foo"] ~=? Nothing
  ]

defaultMatchesTest =
  let parseCmdLine = makeCmdLineParser $ makeOpts [] [testMatch]
      testMatch _ = Just "TEST"
  in [
    "default matches" ~: do Just ms <- return $ matches <$> parseCmdLine []
                            ms <*> [1] @=? [Just "TEST"]
  ]

patternParseTest =
  let parseCmdLine = makeCmdLineParser $ makeOpts [parseTestMatch] []
      parseTestMatch ["test", val] = [\_ -> Just val]
      parseTestMatch _ = []
  in [
    "parsed matches" ~: do Just ms <- return $ matches <$> parseCmdLine ["-p", "test:foo"]
                           ms <*> [1] @=? [Just "foo"],
    -- Here again parseCmdLine returns Nothing, but Maybe CmdOptions isn't
    -- in Eq, so we can't ~=? it. We fmap (length . matches) just to get
    -- something in Eq
    "unparseable pattern" ~: length . matches <$> parseCmdLine ["-p", "2:foo"] ~=? Nothing
  ]

optionsTests = [
    "minimal options" ~: minimalOptionsTest,
    "default matches" ~: defaultMatchesTest,
    "pattern parsing" ~: patternParseTest
  ]
