module Main where

import Test.HUnit
import Test.QuickCheck
import Wheel

prop_test :: Int -> Property
prop_test num = num > 0 ==> num + 1 > 0

test1 :: IO ()
test1 = do
    quickCheck prop_test

test2 = TestCase (assertEqual "Test forward: " (Just 'B') (forward (makeWheelState "BA" (Notch 'A') (Turnover 'A') (Offset 0)) 'A'))

test3 = TestCase (assertEqual "Test backward: " (Just 'X') (backward (makeWheelState "BA" (Notch 'A') (Turnover 'A') (Offset 0)) 'A'))

tests = TestList [test2, test3]

main = test1 >> runTestTT tests