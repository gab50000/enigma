module Main where

import Control.Monad (guard)
import Control.Monad.State.Lazy
import Data.Char (ord)

import qualified Enigma as E
import qualified Wheel as W

runEnigma :: [Char] -> Maybe ([Char], E.Enigma)
runEnigma input = runStateT (forM input E.step) enigma
 where
  enigma = E.initEnigma W.w1 (W.Offset 0) W.w2 (W.Offset 0) W.w3 (W.Offset 0) W.ukwa (E.Steckerbrett [])

main :: IO ()
main = do
  case runEnigma "HELLOWORLD" of
    Just (encoded, _) -> do
      putStrLn encoded
      case runEnigma encoded of
        Just (decoded, _) -> do
          putStrLn decoded
        _ -> putStrLn "oops"
    _ -> putStrLn "oops"
  return ()
