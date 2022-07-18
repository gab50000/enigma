module Main where

import Control.Monad (join)
import Debug.Trace (trace)
import Enigma (Steckerbrett (Steckerbrett), initEnigma, runEnigma)
import Test.HUnit
import Test.QuickCheck (
    Arbitrary (arbitrary),
    Gen,
    elements,
    generate,
    listOf,
    quickCheck,
    sublistOf,
 )
import Test.QuickCheck.Property (failed, reason, succeeded)
import Wheel

newtype InputAlphabet = InputAlphabet {inpAlphToChar :: Char} deriving (Show)
newtype InputString = InputString String deriving (Show)

instance Arbitrary InputAlphabet where
    arbitrary = InputAlphabet <$> elements ['A' .. 'Z']

instance Arbitrary InputString where
    arbitrary = InputString <$> sublistOf ['A' .. 'Z']

simpleWheelState = makeWheelState "CAB" (charToNotch 'A') (charToTurnover 'A') (Offset 0)
whlState = WheelState w1 (Offset 0)

enigma = initEnigma w1 (Offset 0) w2 (Offset 0) w3 (Offset 0) ukwa (Steckerbrett [])

prop_test1 (InputAlphabet char) = (forward whlState char >>= backward whlState) == Just char

prop_test2 :: InputString -> Int -> Int -> Int -> Bool
prop_test2 (InputString message) offset1 offset2 offset3 =
    let enigma = initEnigma w1 (Offset offset1) w2 (Offset offset2) w3 (Offset offset3) ukwa (Steckerbrett [])
        encoded = runEnigma enigma message
        decoded = case encoded of
            Just jEncoded -> runEnigma enigma jEncoded
            _ -> Nothing
     in Just message == decoded

ptest1 = quickCheck prop_test1
ptest2 = quickCheck prop_test2

test2 = TestCase (assertEqual "Test forward: " (Just 'C') (forward simpleWheelState 'A'))
test3 = TestCase (assertEqual "Test backward: " (Just 'C') (backward simpleWheelState 'B'))
test4 = TestCase (assertEqual "Test backward: " (Just 'A') (backward simpleWheelState 'C'))

tests = TestList [test2, test3, test4]

main = do
    ptest1
    ptest2
    runTestTT tests