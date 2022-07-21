module Main where

import Control.Monad (guard)
import Control.Monad.State.Lazy
import Data.Char (ord)

import qualified Data.Maybe
import qualified Enigma as E
import qualified GHC.Base as W
import qualified SteckerBrett as SB
import System.Environment (getArgs)
import qualified Wheel as W

decryptMsg :: Int -> Int -> Int -> Int -> Int -> Int -> String -> String
decryptMsg rp1 rp2 rp3 o1 o2 o3 msg =
    let enigma = E.initEnigma W.w2 (W.RingPos rp1) (W.Offset o1) W.w1 (W.RingPos rp2) (W.Offset o2) W.w3 (W.RingPos rp3) (W.Offset o3) W.ukwa (SB.fromList [('A', 'M'), ('F', 'I'), ('N', 'V'), ('P', 'S'), ('T', 'U'), ('W', 'Z')])
        out = E.runEnigma enigma msg
     in show o1 ++ "-" ++ show o2 ++ "-" ++ show o3 ++ ":" ++ Data.Maybe.fromMaybe "" out

testSettings :: String -> [String]
testSettings encryptedMessage = do
    o1 <- [0 .. 25]
    o2 <- [0 .. 25]
    o3 <- [0 .. 25]

    let msg = decryptMsg 24 13 22 o1 o2 o3 encryptedMessage
    return msg

main :: IO ()
main = do
    let encryptedMessage = "GCDSEAHUGWTQGRKVLFGXUCALXVYMIGMMNMFDXTGNVHVRMMEVOUYFZSLRHDRRXFJWCFHUHMUNZEFRDISIKBGPMYVXUZ"
    -- mapM_ putStrLn (testSettings encryptedMessage)

    putStrLn $ decryptMsg 24 13 22 5 2 20 encryptedMessage
