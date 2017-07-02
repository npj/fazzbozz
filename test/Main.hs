import System.Exit
import Test.HUnit

import TestFazzbozz
import TestCmdOptions

suite = test [
    "fazzbozz" ~: fazzbozzTests,
    "options" ~: optionsTests
  ]

main = do
  results <- runTestTT suite
  if errors results + failures results > 0
    then exitFailure
    else exitSuccess
