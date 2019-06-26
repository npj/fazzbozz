module TestMatches (fazzbozzMatchTests) where

import qualified Data.Map as Map
import Test.HUnit

import Fazzbozz.Base
import Fazzbozz.Matches

moduloTests = [
    "positive" ~: isModulo 3 6 ~=? True,
    "negative" ~: isModulo 3 5 ~=? False
  ]

moduloStateTests = [
    "positive" ~: ModuloState 3 `matchFazz` 6 ~=? (ModuloState 3, True),
    "negative" ~: ModuloState 3 `matchFazz` 5 ~=? (ModuloState 3, False)
  ]

fibonacciTests = [
    "positive" ~: isFibonacci 21 ~=? True,
    "negative" ~: isFibonacci 20 ~=? False
  ]

fibonacciStateTests = [
    "default" ~: let (FibonacciState f) = defaultFibonacciState in take 5 f ~=? [0, 1, 1, 2, 3],
    "positive head" ~: FibonacciState [5, 8, 13, 21] `matchFazz` 5 ~=? (FibonacciState [8, 13, 21], True),
    "positive body" ~: FibonacciState [5, 8, 13, 21] `matchFazz` 8 ~=? (FibonacciState [13, 21], True),
    "negative head" ~: FibonacciState [5, 8, 13, 21] `matchFazz` 4 ~=? (FibonacciState [5, 8, 13, 21], False),
    "negative body" ~: FibonacciState [5, 8, 13, 21] `matchFazz` 10 ~=? (FibonacciState [13, 21], False)
  ]

happyTests = [
    "default positive" ~: isHappy 1 ~=? True,
    "default negative" ~: isHappy 4 ~=? False,
    "accumulated positive" ~: isHappy 7 ~=? True,
    "accumulated negative" ~: isHappy 8 ~=? False
  ]

happyStateTests = [
    "default" ~: defaultHappyState ~=? HappyState (Map.fromList [(1, True), (4, False)]),
    "positive present" ~: defaultHappyState `matchFazz` 1 ~=? (HappyState (Map.fromList [(1, True), (4, False)]), True),
    "positive recurse" ~: defaultHappyState `matchFazz` 10 ~=? (HappyState (Map.fromList [(1, True), (4, False), (10, True)]), True),
    "positive multirecurse" ~: defaultHappyState `matchFazz` 31 ~=? (HappyState (Map.fromList [(1, True), (4, False), (10, True), (31, True)]), True),
    "negative present" ~: defaultHappyState `matchFazz` 4 ~=? (HappyState (Map.fromList [(1, True), (4, False)]), False),
    "negative recurse" ~: defaultHappyState `matchFazz` 20 ~=? (HappyState (Map.fromList [(1, True), (4, False), (20, False)]), False),
    "negative multirecurse" ~: defaultHappyState `matchFazz` 42 ~=? (HappyState (Map.fromList [(1, True), (4, False), (20, False), (42, False)]), False)
  ]

fazzbozzMatchTests = [
    "modulo" ~: moduloTests,
    "ModuloState" ~: moduloStateTests,
    "fibonacci" ~: fibonacciTests,
    "FibonacciState" ~: fibonacciStateTests,
    "happy" ~: happyTests,
    "HappyState" ~: happyStateTests
  ]
