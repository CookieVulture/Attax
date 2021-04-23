{-|
Module      : Testing
Description : Simple unit test framework
Author      : Dhairya Patel
License     : AllRightsReserved

This module provides a simple test framework that has no external
dependencies.
-}
module Testing where

import Data.Foldable (traverse_)
import System.Exit

-- | A 'Test' is a 'String' label identifying the thing being tested,
-- and the result of the test after running it. We give labels to
-- tests so we have some idea what they mean.
--
-- In this assignment, we have added a second constructor:
-- 'TestGroup'. It collects several related tests under a common
-- label.
data Test
  = Test String TestResult
  | TestGroup String [Test]

-- | A test either passes, or fails with an error message.
data TestResult = OK | Fail String deriving (Eq, Show)

-- | Test that two things are equal. The first argument is the
-- computed result, the second argument is the value it should be
-- equal to.
--
-- Note that this function will work across many data types, so long
-- both values are of the same type, and the type:
--
-- 1. Supports testing for equality (i.e., you can use the (==)
--    function), and
--
-- 2. Can be printed as a string (i.e., you can see printed values of
--    this type in GHCi.)
--
-- If you want to write tests about types you have defined, you can
-- add `deriving (Eq, Show)` to your type declarations to satisfy
-- these conditions:
--
-- >>> data MyType = A | B | C deriving (Eq, Show)
--
-- Examples:
--
-- >>> assertEqual ("COMP" ++ "1100") "COMP1100"
-- OK
--
-- >>> assertEqual (1 + 2) 3
-- OK
--
-- >>> assertEqual (2 + 2) 5
-- Fail "4 is not equal to\n5"
assertEqual :: (Eq a, Show a) => a -> a -> TestResult
assertEqual actual expected
  | actual == expected = OK
  | otherwise = Fail (show actual ++ " is not equal to\n" ++ show expected)

-- | Test that two things are different. The first argument is the
-- computed result, the second argument is the value it should be
-- different from. Like 'assertEqual', this function works over many
-- types.
assertNotEqual :: (Eq a, Show a) => a -> a -> TestResult
assertNotEqual actual expected
  | actual /= expected = OK
  | otherwise = Fail (show actual ++ " is equal to\n" ++ show expected)

-- | Test that two 'Double's are basically equal. The first argument
-- is the computed result, the second argument is the value it should be
-- close to.
assertApproxEqual :: Double -> Double -> TestResult
assertApproxEqual actual expected
  | abs (actual - expected) < 0.0001 = OK
  | otherwise =
    Fail (show actual ++ " is not approx. equal to\n" ++ show expected)

-- | Run a test. You are not expected to understand how this works.
runTests :: Test -> IO ()
runTests = go 0 where
  go :: Int -> Test -> IO ()
  go indent test = let spaces = replicate indent ' ' in
    case test of
      Test msg OK -> putStrLn (spaces ++ "PASS: " ++ msg)
      Test msg (Fail failMsg) -> do
        putStrLn (spaces ++ "FAIL: " ++ msg)
        putStrLn failMsg
        exitFailure
      TestGroup msg tests -> do
        putStrLn (spaces ++ msg)
        traverse_ (go (indent + 2)) tests
