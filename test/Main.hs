import System.Exit
import Test.HUnit

import TestCmdOptions
import TestFazzbozz
import TestMatching
import TestIntegration

suite = test [
    "options" ~: optionsTests,
    "fazzbozz" ~: fazzbozzTests,
    "matching" ~: matchingTests,
    "integration" ~: integrationTests
  ]

main = do
  results <- runTestTT suite
  if errors results + failures results > 0
    then exitFailure
    else exitSuccess
