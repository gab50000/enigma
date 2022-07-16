module Main where

import Control.Monad (guard)
import Data.Char (ord)
import Wheel

newtype Steckerbrett = Steckerbrett [(Char, Char)]

data Enigma = Enigma WheelState WheelState WheelState Umkehrwalze Steckerbrett

argsort :: [Char] -> [Int]
argsort = go []
  where
    go :: [Int] -> [Char] -> [Int]
    go indices [] = reverse indices
    go indices (x : xs) = go ((ord x - ord 'A') : indices) xs

-- revert :: WheelState -> WheelState
-- revert (WheelState (Wheel translation _ _) offset) = undefined

instance Encryptor Enigma where
    forward (Enigma wst1 wst2 wst3 ukw _) c0 = do
        c1 <- forward wst1 c0
        c2 <- forward wst2 c1
        c3 <- forward wst3 c2
        c4 <- forward ukw c3
        return c3

step :: Enigma -> Char -> (Enigma, Char)
step enigma char = undefined

main :: IO ()
main = print (makeWheel (['B' .. 'Z'] ++ "A") (Notch 'X') (Turnover 'X'))
