module Wheel (
    Notch (..),
    Turnover (..),
    Offset (..),
    Wheel,
    makeWheel,
    WheelState (..),
    Umkehrwalze (..),
    Encryptor (forward),
    forward_,
    backward_,
    revert,
) where

import Data.Char (chr, ord)
import Data.List (elemIndex)

newtype Notch = Notch Char deriving (Show)

newtype Turnover = Turnover Char deriving (Show)

data Wheel = Wheel [Int] Notch Turnover deriving (Show)

newtype Umkehrwalze = Umkehrwalze [Int]

newtype Offset = Offset Int

data WheelState = WheelState Wheel Offset

diff :: [Char] -> [Int]
diff lst =
    let alphabet = ['A' .. 'Z']
     in zipWith (\x y -> ord x - ord y) lst alphabet

makeWheel :: [Char] -> Notch -> Turnover -> Wheel
makeWheel lst = Wheel (diff lst)

makeUmkehrwalze :: [Char] -> Umkehrwalze
makeUmkehrwalze lst = Umkehrwalze (diff lst)

w1 = makeWheel "EKMFLGDQVZNTOWYHXUSPAIBRCJ" (Notch 'Y') (Turnover 'Q')
w2 = makeWheel "AJDKSIRUXBLHWTMCQGZNPYFVOE" (Notch 'M') (Turnover 'E')
w3 = makeWheel "BDFHJLCPRTXVZNYEIWGAKMUSQO" (Notch 'D') (Turnover 'V')
w4 = makeWheel "ESOVPZJAYQUIRHXLNFTGKDCMWB" (Notch 'R') (Turnover 'J')
w5 = makeWheel "VZBRGITYUPSDNHLXAWMJQOFECK" (Notch 'H') (Turnover 'Z')

ukwa = makeUmkehrwalze "EJMZALYXVBWFCRQUONTSPIKHGD"
ukwb = makeUmkehrwalze "YRUHQSLDPXNGOKMIEBFZCWVJAT"
ukwc = makeUmkehrwalze "FVPJIAOYEDRZXWGCTKUQSBNMHL"

class Encryptor a where
    forward :: a -> Char -> Maybe Char
    backward :: a -> Char -> Maybe Char

instance Encryptor WheelState where
    forward (WheelState (Wheel translation _ _) (Offset offset)) char = do
        let rotatedTrans = rotate translation offset
        forward_ rotatedTrans char
    backward (WheelState (Wheel translation _ _) (Offset offset)) char = do
        let rotatedTrans = rotate translation offset
        backward_ rotatedTrans char

instance Encryptor Umkehrwalze where
    forward (Umkehrwalze translation) = forward_ translation

rotate :: [a] -> Int -> [a]
rotate lst off = drop off lst ++ take off lst

forward_ :: [Int] -> Char -> Maybe Char
forward_ translation char = do
    idx <- elemIndex char ['A' .. 'Z']
    let offset = translation !! idx
    let charIdx = ord char
    let newCharIdx = mod (charIdx + offset) (ord 'Z') + (ord 'A' - 1)
    return (chr newCharIdx)

backward_ :: [Int] -> Char -> Maybe Char
backward_ translation char = do
    backTrans <- revert translation
    forward_ backTrans char

revert :: [Int] -> Maybe [Int]
revert lst =
    let indices = [0 .. length lst - 1]
     in mapM (`elemIndex` lst) indices
