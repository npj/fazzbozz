module TestFazzbozz (fazzbozzTests) where

import Control.Monad
import Test.HUnit

import Fazzbozz

neverMatch _ = Nothing
alwaysFoo _ = Just "foo"
evenBar n = "bar" <$ guard (even n)

fazzbozzFunctionTests = [
    "no matches" ~: fazzbozz [] 1 ~=? "1",
    "trivial negative match" ~: fazzbozz [neverMatch] 1 ~=? "1",
    "trivial positive match" ~: fazzbozz [alwaysFoo] 1 ~=? "foo",
    "multiple trivial matches" ~: fazzbozz [neverMatch, alwaysFoo] 1 ~=? "foo",

    "nontrivial negative match" ~: fazzbozz [evenBar] 1 ~=? "1",
    "nontrivial positive match" ~: fazzbozz [evenBar] 2 ~=? "bar",

    "multiple positive matches" ~: fazzbozz [alwaysFoo, evenBar] 2 ~=? "foobar"
  ]

parseCountPatternTests = [
    "parse and positive match" ~: do [match] <- return $ parseCountPattern ["2", "foo"]
                                     match 2 @=? Just "foo",
    "parse and negative match" ~: do [match] <- return $ parseCountPattern ["2", "foo"]
                                     match 3 @=? Nothing,

    "fail parse: nonnumeric" ~: (length $ parseCountPattern ["two", "foo"]) ~=? 0,
    "fail parse: not enough args" ~: (length $ parseCountPattern ["2"]) ~=? 0,
    "fail parse: too many args" ~: (length $ parseCountPattern ["2", "foo", "bar"]) ~=? 0
  ]

parseFibonacciPatternTests = [
    "parse and positive match" ~: do [match] <- return $ parseFibonacciPattern ["fib", "foo"]
                                     match 3 @=? Just "foo",
    "parse and negative match" ~: do [match] <- return $ parseFibonacciPattern ["fib", "foo"]
                                     match 4 @=? Nothing,

    "fail parse: bad label" ~: (length $ parseCountPattern ["fob", "foo"]) ~=? 0
  ]

fazzbozzTests = [
    "fazzbozz function" ~: fazzbozzFunctionTests,
    "parseCountPattern" ~: parseCountPatternTests,
    "parseFibonacciPattern" ~: parseFibonacciPatternTests
  ]
