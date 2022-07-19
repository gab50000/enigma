module Enigma where

import Control.Monad.State (State)
import Control.Monad.State.Lazy (StateT, forM, get, lift, put, runStateT)

import SteckerBrett as SB
import Wheel (Umkehrwalze (Umkehrwalze))
import qualified Wheel as SB
import qualified Wheel as W

data Enigma = Enigma W.WheelState W.WheelState W.WheelState W.Umkehrwalze Steckerbrett deriving (Show)

step :: Char -> StateT Enigma Maybe Char
step c0 = do
    (Enigma wst1 wst2 wst3 ukw stkbr) <- get
    -- pass through Steckerbrett
    c1 <- lift $ SB.forward stkbr c0
    -- pass through wheels
    c2 <- lift $ W.forward wst1 c1 >>= W.forward wst2 >>= W.forward wst3
    -- pass through Umkehrwalze
    c4 <- lift $ W.forward ukw c2
    -- pass through wheels in reverse direction
    c5 <- lift $ W.forward (W.revert wst3) c4 >>= W.forward (W.revert wst2) >>= W.forward (W.revert wst1)
    -- pass through Steckerbrett again
    c6 <- lift $ SB.forward stkbr c5

    let wst3New = W.rotate wst3
    let wst2New = if W.isAtTurnover wst3 then W.rotate wst2 else wst2
    let wst1New = if W.isAtTurnover wst2 then W.rotate wst1 else wst1

    put (Enigma wst1New wst2New wst3New ukw stkbr)
    return c5

initEnigma :: W.Wheel -> W.Offset -> W.Wheel -> W.Offset -> W.Wheel -> W.Offset -> W.Umkehrwalze -> Steckerbrett -> Enigma
initEnigma whl1 offset1 whl2 offset2 whl3 offset3 = Enigma (W.WheelState whl1 offset1) (W.WheelState whl2 offset2) (W.WheelState whl3 offset3)

runEnigma :: Enigma -> [Char] -> Maybe [Char]
runEnigma enigma input = do
    (encoded, _) <- runStateT (forM input step) enigma
    return encoded