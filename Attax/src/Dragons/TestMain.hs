module Dragons.TestMain where

import AITests
import AtaxxTests
import Testing

allTests :: Test
allTests = TestGroup "All Tests"
  [ ataxxTests
  , aiTests
  ]

testMain :: IO ()
testMain = runTests allTests
