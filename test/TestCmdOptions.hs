module TestCmdOptions (optionsTests) where

import Test.HUnit
import Options.Applicative

import CmdOptions

parseCmdLine :: [String] -> Maybe CmdOptions
parseCmdLine = getParseResult <$> execParserPure defaultPrefs opts

runTest :: (Eq t, Show t) => (CmdOptions -> t) -> String -> [String] -> Maybe t -> Test
runTest field name cmdLine expect = name ~: field <$> parseCmdLine cmdLine ~=? expect

numberTest = [
    test "default number" [] $ Just 20,
    test "specify number" ["-n", "100"] $ Just 100,

    test "invalid number" ["-n", "one hundred"] Nothing,
    test "missing number" ["-n"] Nothing
  ]
  where test = runTest number

patternsTest = [
    test "default patterns" [] $ Just [ModuloMatch 3 "fazz", ModuloMatch 5 "bozz"],

    test "modulo pattern" ["-p", "2:test"] $ Just [ModuloMatch 2 "test"],
    test "modulo missing label" ["-p", "2"] Nothing,
    test "modulo extra args" ["-p", "2:a:b"] Nothing,

    test "fibonacci pattern" ["-p", "fib:test"] $ Just [FibonacciMatch "test"],
    test "fibonacci missing label" ["-p", "fib"] Nothing,
    test "fibonacci extra args" ["-p", "fib:a:b"] Nothing,

    test "multiple patterns" ["-p", "2:a", "-p", "fib:b"] $
      Just [ModuloMatch 2 "a", FibonacciMatch "b"],
    test "invalid pattern type" ["-p", "x:test"] Nothing
  ]
  where test = runTest matchSpecs

optionsTests = [
    "number" ~: numberTest,
    "patterns" ~: patternsTest
  ]
