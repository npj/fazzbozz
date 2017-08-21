module TestFazzbozz (fazzbozzTests) where

import Test.HUnit
import Fazzbozz

basicMatches = [
    simpleMatch 3 "fazz",
    simpleMatch 5 "bozz"
  ]

fazzbozzTests = test [
    fazzbozz basicMatches 1 ~?= "1",
    fazzbozz basicMatches 3 ~?= "fazz",
    fazzbozz basicMatches 5 ~?= "bozz",
    fazzbozz basicMatches 6 ~?= "fazz",
    fazzbozz basicMatches 7 ~?= "7",
    fazzbozz basicMatches 15 ~?= "fazzbozz"
  ]
