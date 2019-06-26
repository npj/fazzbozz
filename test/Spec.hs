import System.Exit
import Test.HUnit

import TestCmdOptions
import TestCore
import TestIntegration
import TestMatches
import TestSimple

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
