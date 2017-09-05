module TestMatching (matchingTests) where

import Test.HUnit
import Matching

matchingTests =
  let match = simpleMatch 3 "foo"
  in [
    "match num" ~: match 3 ~=? Just "foo",
    "match multiple" ~: match 6 ~=? Just "foo",
    "miss other num" ~: match 2 ~=? Nothing
  ]
