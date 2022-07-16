module Main where

import Test.HUnit
import Test.QuickCheck
import Wheel

prop_test :: Int -> Property
prop_test num = num > 0 ==> num + 1 > 0

test1 :: IO ()
test1 = do
    quickCheck prop_test

test2 = TestCase (assertEqual "Test forward_: " (Just 'B') (forward_ [1] 'A'))
test3 = TestCase (assertEqual "Test backward_: " (Just 'A') (backward_ [1] 'B'))

tests = TestList [test2, test3]

main = test1 >> runTestTT tests