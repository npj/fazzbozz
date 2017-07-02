module TestFazzbozz (fazzbozzTests) where

import Test.HUnit
import Fazzbozz

fazzbozzTests = test [
    fazzbozz 1 ~?= "1",
    fazzbozz 3 ~?= "fazz",
    fazzbozz 5 ~?= "bozz",
    fazzbozz 6 ~?= "fazz",
    fazzbozz 7 ~?= "7",
    fazzbozz 15 ~?= "fazzbozz"
  ]
