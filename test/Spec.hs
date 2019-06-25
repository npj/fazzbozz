import System.Exit
import Test.HUnit

import TestCmdOptions
import TestFazzbozzCore
import TestFazzbozzMatches
import TestFazzbozzSimple
import TestIntegration

suite = test [
    "options" ~: optionsTests,
    "fazzbozz core" ~: fazzbozzCoreTests,
    "fazzbozz matches" ~: fazzbozzMatchTests,
    "fazzbozz simple" ~: fazzbozzSimpleTests,
    "integration" ~: integrationTests
  ]

main = do
  results <- runTestTT suite
  if errors results + failures results > 0
    then exitFailure
    else exitSuccess
