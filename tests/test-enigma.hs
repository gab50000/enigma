module Main where

import Control.Monad (join)
import Debug.Trace (trace)
import Enigma (Steckerbrett (Steckerbrett), initEnigma)
import Test.HUnit
import Test.QuickCheck (
    Arbitrary (arbitrary),
    Gen,
    elements,
    generate,
    quickCheck,
 )
import Test.QuickCheck.Property (failed, reason, succeeded)
import Wheel

newtype InputAlphabet = InputAlphabet Char deriving (Show)

instance Arbitrary InputAlphabet where
    arbitrary = InputAlphabet <$> elements ['A' .. 'Z']

simpleWheelState = makeWheelState "CAB" (charToNotch 'A') (charToTurnover 'A') (Offset 0)
whlState = WheelState w1 (Offset 0)

enigma = initEnigma w1 (Offset 0) w2 (Offset 0) w3 (Offset 0) ukwa (Steckerbrett [])

prop_test1 (InputAlphabet char) = (forward whlState char >>= backward whlState) == Just char

ptest1 = quickCheck prop_test1

test2 = TestCase (assertEqual "Test forward: " (Just 'C') (forward simpleWheelState 'A'))
test3 = TestCase (assertEqual "Test backward: " (Just 'C') (backward simpleWheelState 'B'))
test4 = TestCase (assertEqual "Test backward: " (Just 'A') (backward simpleWheelState 'C'))

tests = TestList [test2, test3, test4]

main = do
    ptest1
    runTestTT tests