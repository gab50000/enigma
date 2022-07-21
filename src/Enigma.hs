module Enigma where

import Control.Monad.State (State)
import Control.Monad.State.Lazy (StateT, forM, get, lift, put, runStateT)

import Debug.Trace (trace)
import qualified GHC.Base as W
import SteckerBrett as SB
import Wheel (RingPos (..), Umkehrwalze (..), setRingPosition)
import qualified Wheel as SB
import qualified Wheel as W

data Enigma = Enigma W.WheelState W.WheelState W.WheelState W.Umkehrwalze Steckerbrett deriving (Show)

step :: Char -> StateT Enigma Maybe Char
step c0 = do
    enigma <- get
    let newEnigma@(Enigma wst1 wst2 wst3 ukw stkbr) = updateState enigma
    put newEnigma
    -- pass through Steckerbrett
    c1 <- lift $ SB.forward stkbr c0
    -- pass through wheels going from right to left
    c2 <- lift $ W.forward wst3 c1 >>= W.forward wst2 >>= W.forward wst1
    -- pass through Umkehrwalze
    c3 <- lift $ W.forward ukw c2
    -- pass through wheels in reverse direction
    c4 <- lift $ W.forward (W.revert wst1) c3 >>= W.forward (W.revert wst2) >>= W.forward (W.revert wst3)
    -- pass through Steckerbrett again
    lift $ SB.forward stkbr c4

updateState :: Enigma -> Enigma
updateState (Enigma wst1 wst2 wst3 ukw stkbr) =
    let wst3New = trace "Rotate wheel 3" W.rotate wst3
        wst2New = if W.isAtTurnover wst3 then trace "Rotate wheel 2" W.rotate wst2 else wst2
        wst1New = if W.isAtTurnover wst2 then trace "Rotate wheel 1" W.rotate wst1 else wst1
     in Enigma wst1New wst2New wst3New ukw stkbr

initEnigma :: W.Wheel -> W.RingPos -> W.Offset -> W.Wheel -> W.RingPos -> W.Offset -> W.Wheel -> W.RingPos -> W.Offset -> W.Umkehrwalze -> Steckerbrett -> Enigma
initEnigma whl1 ringPos1 offset1 whl2 ringPos2 offset2 whl3 ringPos3 offset3 =
    let whl1' = setRingPosition whl1 ringPos1
        whl2' = setRingPosition whl2 ringPos2
        whl3' = setRingPosition whl3 ringPos3
     in Enigma (W.WheelState whl1' offset1) (W.WheelState whl2' offset2) (W.WheelState whl3' offset3)

runEnigma :: Enigma -> [Char] -> Maybe [Char]
runEnigma enigma input = do
    (encoded, _) <- runStateT (forM input step) enigma
    return encoded