module Main where

newtype Notch = Notch Char

newtype Turnover = Turnover Char

data Wheel = Wheel [Char] Notch Turnover

newtype Umkehrwalze = Umkehrwalze [Char]

data Steckerbrett = Steckerbrett

newtype Offset = Offset Int

data WheelState = WheelState Wheel Offset

data Enigma = Enigma WheelState WheelState WheelState Umkehrwalze Steckerbrett

w1 = Wheel "EKMFLGDQVZNTOWYHXUSPAIBRCJ" (Notch 'Y') (Turnover 'Q')
w2 = Wheel "AJDKSIRUXBLHWTMCQGZNPYFVOE" (Notch 'M') (Turnover 'E')
w3 = Wheel "BDFHJLCPRTXVZNYEIWGAKMUSQO" (Notch 'D') (Turnover 'V')
w4 = Wheel "ESOVPZJAYQUIRHXLNFTGKDCMWB" (Notch 'R') (Turnover 'J')
w5 = Wheel "VZBRGITYUPSDNHLXAWMJQOFECK" (Notch 'H') (Turnover 'Z')

ukwa = Umkehrwalze "EJMZALYXVBWFCRQUONTSPIKHGD"
ukwb = Umkehrwalze "YRUHQSLDPXNGOKMIEBFZCWVJAT"
ukwc = Umkehrwalze "FVPJIAOYEDRZXWGCTKUQSBNMHL"

step :: Wheel -> Char -> Char
step = undefined

main :: IO ()
main = putStrLn "Engima"
