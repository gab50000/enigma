module Enigma where

import qualified Wheel as W

newtype Steckerbrett = Steckerbrett [(Char, Char)] deriving (Show)

data Enigma = Enigma W.WheelState W.WheelState W.WheelState W.Umkehrwalze Steckerbrett deriving (Show)

step :: Enigma -> Char -> Maybe (Enigma, Char)
step enigma@(Enigma wst1 wst2 wst3 ukw stkbr) c0 = do
    c1 <- W.forward wst1 c0
    c2 <- W.forward wst2 c1
    c3 <- W.forward wst3 c2
    c4 <- W.forward ukw c3
    c5 <- W.backward wst3 c4
    c6 <- W.backward wst2 c5
    c7 <- W.backward wst1 c6

    let wst3New = W.rotate wst3
    let wst2New = if W.isAtTurnover wst3 then W.rotate wst2 else wst2
    let wst1New = if W.isAtTurnover wst2 then W.rotate wst1 else wst1
    return (Enigma wst1New wst2New wst3New ukw stkbr, c7)

initEnigma :: W.Wheel -> W.Offset -> W.Wheel -> W.Offset -> W.Wheel -> W.Offset -> W.Umkehrwalze -> Steckerbrett -> Enigma
initEnigma whl1 offset1 whl2 offset2 whl3 offset3 = Enigma (W.WheelState whl1 offset1) (W.WheelState whl2 offset2) (W.WheelState whl3 offset3)