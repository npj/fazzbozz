module TestMatching (matchingTests) where

import Test.HUnit
import Matching

simpleMatchTests =
  let match = simpleMatch 3 "foo"
  in [
    "match num" ~: match 3 ~=? Just "foo",
    "match multiple" ~: match 6 ~=? Just "foo",
    "miss other num" ~: match 2 ~=? Nothing
  ]

fibonacciTests =
  let match = matchFibonacci "fib"
  in [
    "positive" ~: match 3 ~=? Just "fib",
    "negative" ~: match 4 ~=? Nothing
  ]

matchingTests = [
    "simpleMatch" ~: simpleMatchTests,
    "fibonacci" ~: fibonacciTests
  ]
