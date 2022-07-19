module Wheel (
    Notch,
    Turnover,
    charToNotch,
    charToTurnover,
    Offset (..),
    Wheel,
    makeWheel,
    makeWheelState,
    WheelState (..),
    Umkehrwalze (..),
    Encryptor (forward),
    Reversible (revert),
    w1,
    w2,
    w3,
    w4,
    w5,
    ukwa,
    rotate,
    isAtTurnover,
) where

import Control.Monad (guard)
import Data.Char (chr, ord)
import Data.List (elemIndex, sort)
import Debug.Trace (trace)

newtype Notch = Notch Int deriving (Show)

charToNotch :: Char -> Notch
charToNotch char = Notch (ord char - ord 'A')

newtype Turnover = Turnover Int deriving (Show)

charToTurnover :: Char -> Turnover
charToTurnover char = Turnover (ord char - ord 'A')

data Wheel = Wheel RelTranslation Notch Turnover deriving (Show)

newtype Umkehrwalze = Umkehrwalze RelTranslation deriving (Show)

newtype Offset = Offset Int deriving (Show)

data WheelState = WheelState Wheel Offset deriving (Show)

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

isAtTurnover :: WheelState -> Bool
isAtTurnover (WheelState (Wheel _ _ (Turnover turnover)) (Offset offset)) = turnover == offset

makeUmkehrwalze :: [Char] -> Umkehrwalze
makeUmkehrwalze lst = Umkehrwalze (diff lst)

w1 = makeWheel "EKMFLGDQVZNTOWYHXUSPAIBRCJ" (charToNotch 'Y') (charToTurnover 'Q')
w2 = makeWheel "AJDKSIRUXBLHWTMCQGZNPYFVOE" (charToNotch 'M') (charToTurnover 'E')
w3 = makeWheel "BDFHJLCPRTXVZNYEIWGAKMUSQO" (charToNotch 'D') (charToTurnover 'V')
w4 = makeWheel "ESOVPZJAYQUIRHXLNFTGKDCMWB" (charToNotch 'R') (charToTurnover 'J')
w5 = makeWheel "VZBRGITYUPSDNHLXAWMJQOFECK" (charToNotch 'H') (charToTurnover 'Z')

ukwa = makeUmkehrwalze "EJMZALYXVBWFCRQUONTSPIKHGD"
ukwb = makeUmkehrwalze "YRUHQSLDPXNGOKMIEBFZCWVJAT"
ukwc = makeUmkehrwalze "FVPJIAOYEDRZXWGCTKUQSBNMHL"

class Encryptor a where
    forward :: a -> Char -> Maybe Char
    backward :: a -> Char -> Maybe Char

class Reversible a where
    revert :: a -> a

instance Encryptor WheelState where
    forward (WheelState (Wheel translation _ _) (Offset offset)) char = do
        let rotatedTrans = setOffset translation offset
        forward_ rotatedTrans char

instance Reversible WheelState where
    revert (WheelState (Wheel translation notch turnover) offset) =
        let reverseTranslation = revert translation
         in WheelState (Wheel reverseTranslation notch turnover) offset

instance Encryptor Umkehrwalze where
    forward (Umkehrwalze translation) = forward_ translation

setOffset :: RelTranslation -> Int -> RelTranslation
setOffset (RelTranslation lst) off =
    let offset = mod off (length lst)
     in RelTranslation (drop offset lst ++ take offset lst)

rotate :: WheelState -> WheelState
rotate (WheelState whl (Offset offset)) = WheelState whl (Offset (offset + 1))

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

instance Reversible RelTranslation where
    revert (RelTranslation offsets) =
        let indices = map ord ['A' .. 'Z']
            absTranslation = zipWith translateIdx indices offsets
            mapping = sort (zipWith (curry (\t -> (snd t, fst t))) indices absTranslation)
         in RelTranslation $ map (\t -> snd t - fst t) mapping
