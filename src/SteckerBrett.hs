module SteckerBrett where

import qualified Data.HashMap.Strict as HM
import Wheel (Encryptor (forward))

newtype Steckerbrett = Steckerbrett (HM.HashMap Char Char) deriving (Show)

fromList :: [(Char, Char)] -> Steckerbrett
fromList lst =
    let mapping = HM.fromList lst
        reverseMapping = HM.fromList $ map (\tup -> (snd tup, fst tup)) lst
     in Steckerbrett (HM.union mapping reverseMapping)

instance Encryptor Steckerbrett where
    forward (Steckerbrett sb) char = Just $ HM.findWithDefault char char sb
