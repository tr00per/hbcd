module Data.Serialization.BCD.Telephony
    ( SimpleHex (..)
    , BCD (..)
    , Telephony
    ) where

import           Data.Serialization.BCD.Common

newtype Telephony = Telephony Bytes deriving (Show, Eq)

instance SimpleHex Telephony where
    toHex (Telephony bs) = toHex bs
    fromHex = Telephony . fromHex

instance BCD Telephony where
    code = Telephony . pack backPadding swapPair . digits

    decode (Telephony b) = number $ unpack swapPair b
