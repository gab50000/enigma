module Wheel (
    Notch (..),
    Turnover (..),
    Offset (..),
    Wheel,
    makeWheel,
    makeWheelState,
    WheelState (..),
    Umkehrwalze (..),
    Encryptor (forward, backward),
) where

import Data.Char (chr, ord)
import Data.List (elemIndex)
import Debug.Trace (trace)

newtype Notch = Notch Char deriving (Show)

newtype Turnover = Turnover Char deriving (Show)

data Wheel = Wheel RelTranslation Notch Turnover deriving (Show)

newtype Umkehrwalze = Umkehrwalze RelTranslation

newtype Offset = Offset Int

data WheelState = WheelState Wheel Offset

newtype RelTranslation = RelTranslation [Int] deriving (Show)

newtype AbsTranslation = AbsTranslation [Int] deriving (Show)

diff :: [Char] -> RelTranslation
diff lst =
    let alphabet = ['A' .. 'Z']
     in RelTranslation (zipWith (\x y -> ord x - ord y) lst alphabet)

makeWheel :: [Char] -> Notch -> Turnover -> Wheel
makeWheel lst = Wheel (diff lst)

makeWheelState :: String -> Notch -> Turnover -> Offset -> WheelState
makeWheelState lst notch turnover = WheelState (makeWheel lst notch turnover)

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

rotate :: RelTranslation -> Int -> RelTranslation
rotate (RelTranslation lst) off = RelTranslation (drop off lst ++ take off lst)

forward_ :: RelTranslation -> Char -> Maybe Char
forward_ (RelTranslation translation) char = do
    idx <- elemIndex char ['A' .. 'Z']
    let offset = translation !! idx
    return (translateChar char offset)

backward_ :: RelTranslation -> Char -> Maybe Char
backward_ translation char = do
    let backTrans = revert translation
    forward_ backTrans char

translateChar :: Char -> Int -> Char
translateChar char offset =
    let charIdx = ord char
     in chr (translateIdx charIdx offset)

translateIdx :: Int -> Int -> Int
translateIdx charIdx offset = mod (charIdx + offset - ord 'A') 26 + ord 'A'

revert :: RelTranslation -> RelTranslation
revert (RelTranslation offsets) =
    let indices = [0 ..]
        absTranslation = zipWith translateIdx indices offsets
     in RelTranslation $ zipWith (-) absTranslation indices
