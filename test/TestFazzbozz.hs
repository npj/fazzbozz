module TestFazzbozz (fazzbozzTests) where

import Control.Monad
import Test.HUnit

import Fazzbozz
import CmdOptions

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

moduloTests = [
    "positive" ~: matchModulo 3 "foo" 6 ~=? Just "foo",
    "negative" ~: matchModulo 3 "foo" 5 ~=? Nothing
  ]

fibonacciTests = [
    "positive" ~: matchFibonacci "foo" 21 ~=? Just "foo",
    "negative" ~: matchFibonacci "foo" 20 ~=? Nothing
  ]

fazzbozzTests = [
    "fazzbozz" ~: fazzbozzFunctionTests,
    "modulo" ~: moduloTests,
    "fibonacci" ~: fibonacciTests
  ]
