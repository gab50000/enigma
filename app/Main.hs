module Main where

import Control.Monad (guard)
import Control.Monad.State.Lazy
import Data.Char (ord)

import qualified Enigma as E
import qualified Wheel as W

main :: IO ()
main = do
  let enigma = E.initEnigma W.w1 (W.Offset 0) W.w2 (W.Offset 0) W.w3 (W.Offset 0) W.ukwa (E.Steckerbrett [])
  case E.runEnigma enigma "HELLOWORLD" of
    Just encoded -> do
      putStrLn encoded
      case E.runEnigma enigma encoded of
        Just decoded -> do
          putStrLn decoded
        _ -> putStrLn "oops"
    _ -> putStrLn "oops"
  return ()
