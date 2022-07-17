module Main where

import Control.Monad (guard)
import Data.Char (ord)

import qualified Enigma as E
import qualified Wheel as W

main :: IO ()
main = print (W.makeWheel (['B' .. 'Z'] ++ "A") (W.charToNotch 'X') (W.charToTurnover 'X'))
