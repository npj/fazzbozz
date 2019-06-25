module TestFazzbozzCore (fazzbozzCoreTests) where

import Test.HUnit

import Fazzbozz.Core

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

fazzbozzCoreTests = [
    "statefulScan" ~: statefulScanTests
  ]
