module Main where

import Control.Monad (guard)
import Control.Monad.State.Lazy
import Data.Char (ord)

import qualified Data.Maybe
import qualified Enigma as E
import qualified SteckerBrett as SB
import System.Environment (getArgs)
import qualified Wheel as W

decryptMsg :: Int -> Int -> Int -> String
decryptMsg o1 o2 o3 =
    let enigma = E.initEnigma W.w2 (W.Offset o1) W.w1 (W.Offset o2) W.w3 (W.Offset o3) W.ukwa (SB.fromList [('A', 'M'), ('F', 'I'), ('N', 'V'), ('P', 'S'), ('T', 'U'), ('W', 'Z')])
        encryptedMessage = "GCDSEAHUGWTQGRKVLFGXUCALXVYMIGMMNMFDXTGNVHVRMMEVOUYFZSLRHDRRXFJWCFHUHMUNZEFRDISIKBGPMYVXUZ"
        decryptedMessage = E.runEnigma enigma encryptedMessage
     in show o1 ++ "-" ++ show o2 ++ "-" ++ show o3 ++ ":" ++ Data.Maybe.fromMaybe "" decryptedMessage

testSettings :: [String]
testSettings = do
    o1 <- [0 .. 25]
    o2 <- [0 .. 25]
    o3 <- [0 .. 25]

    let msg = decryptMsg o1 o2 o3
    return msg
enigma = E.initEnigma W.w3 (W.Offset 5) W.w2 (W.Offset 5) W.w1 (W.Offset 5) W.ukwb (SB.fromList [])

main :: IO ()
main = putStrLn $ decryptMsg 3 15 16
