module Main where

import Control.Monad (join)
import Debug.Trace (trace)
import Enigma (initEnigma, runEnigma)
import SteckerBrett as SB
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
import Wheel (Encryptor (forward))

newtype InputAlphabet = InputAlphabet {inpAlphToChar :: Char} deriving (Show)
newtype InputString = InputString String deriving (Show)

instance Arbitrary InputAlphabet where
    arbitrary = InputAlphabet <$> elements ['A' .. 'Z']

instance Arbitrary InputString where
    arbitrary = InputString <$> sublistOf ['A' .. 'Z']

simpleWheelState = makeWheelState "CAB" (charToNotch 'A') (charToTurnover 'A') (Offset 0)
whlState = WheelState w1 (Offset 0)

enigma = initEnigma w1 (Offset 0) w2 (Offset 0) w3 (Offset 0) ukwa (SB.fromList [])

prop_test1 (InputAlphabet char) = (forward whlState char >>= forward (revert whlState)) == Just char

prop_test2 :: InputString -> Int -> Int -> Int -> Bool
prop_test2 (InputString message) offset1 offset2 offset3 =
    let enigma = initEnigma w1 (Offset offset1) w2 (Offset offset2) w3 (Offset offset3) ukwa (SB.fromList [])
        encoded = runEnigma enigma message
        decoded = case encoded of
            Just jEncoded -> runEnigma enigma jEncoded
            _ -> Nothing
     in Just message == decoded

ptest1 = quickCheck prop_test1
ptest2 = quickCheck prop_test2

test2 = TestCase (assertEqual "Test forward: " (Just 'C') (forward simpleWheelState 'A'))
test3 = TestCase (assertEqual "Test backward: " (Just 'C') (forward (revert simpleWheelState) 'B'))
test4 = TestCase (assertEqual "Test backward: " (Just 'A') (forward (revert simpleWheelState) 'C'))

testTrueSetting1 =
    let enigma = initEnigma w2 (Offset 24) w1 (Offset 13) w3 (Offset 22) ukwa (SB.fromList [('A', 'M'), ('F', 'I'), ('N', 'V'), ('P', 'S'), ('T', 'U'), ('W', 'Z')])
        encryptedMessage = "GCDSEAHUGWTQGRKVLFGXUCALXVYMIGMMNMFDXTGNVHVRMMEVOUYFZSLRHDRRXFJWCFHUHMUNZEFRDISIKBGPMYVXUZ"
        decryptedMessage = runEnigma enigma encryptedMessage
     in TestCase (assertEqual "Decrpyted message is correct: " (Just "bla") decryptedMessage)

tests = TestList [test2, test3, test4, testTrueSetting1]

main = do
    ptest1
    ptest2
    runTestTT tests