module TestFazzbozz (fazzbozzTests) where

import Control.Monad
import Test.HUnit

import Fazzbozz
import CmdOptions

fazzbozzFunctionTests = [
    "no matches" ~: fazzbozz [] 1 ~=? "1",
    "trivial negative match" ~: fazzbozz [neverMatch] 1 ~=? "1",
    "trivial positive match" ~: fazzbozz [alwaysFoo] 1 ~=? "foo",
    "multiple trivial matches" ~: fazzbozz [neverMatch, alwaysFoo] 1 ~=? "foo",

    "nontrivial negative match" ~: fazzbozz [evenBar] 1 ~=? "1",
    "nontrivial positive match" ~: fazzbozz [evenBar] 2 ~=? "bar",

    "multiple positive matches" ~: fazzbozz [alwaysFoo, evenBar] 2 ~=? "foobar"
  ]
  where
    neverMatch = ("x", const False)
    alwaysFoo = ("foo", const True)
    evenBar = ("bar", even)

statefulScanTests = [
    "collect" ~: statefulScan collect [] [1,2,3] ~=? [[1], [1,2], [1,2,3]],
    "product of last two" ~: statefulScan productTwo 0 [1,2,3,4] ~=? [0,2,6,12],
    "check word prefixes" ~: statefulScan checkWord "" "android" ~=?
      [True,True,True,False,False,False,True]
  ]
  where
    collect st v = (st', st')
      where st' = st ++ [v]
    productTwo last v = (v, last * v)
    checkWord w c = (newWord, newWord `elem` ["a", "an", "and", "android"])
      where
        newWord = w ++ [c]

moduloTests = [
    "positive" ~: isModulo 3 6 ~=? True,
    "negative" ~: isModulo 3 5 ~=? False
  ]

fibonacciTests = [
    "positive" ~: isFibonacci 21 ~=? True,
    "negative" ~: isFibonacci 20 ~=? False
  ]

happyTests = [
    "default positive" ~: isHappy 1 ~=? True,
    "default negative" ~: isHappy 4 ~=? False,
    "accumulated positive" ~: isHappy 7 ~=? True,
    "accumulated negative" ~: isHappy 8 ~=? False
  ]

fazzbozzTests = [
    "fazzbozz" ~: fazzbozzFunctionTests,
    "statefulScan" ~: statefulScanTests,
    "modulo" ~: moduloTests,
    "fibonacci" ~: fibonacciTests,
    "happy" ~: happyTests
  ]
