module Data.Serialization.BCD.Simple
    ( SimpleHex (..)
    , BCD (..)
    , Simple
    ) where

import           Data.Serialization.BCD.Common

newtype Simple = Simple Bytes deriving (Show, Eq)

instance SimpleHex Simple where
    toHex (Simple bs) = toHex bs
    fromHex = Simple . fromHex

instance BCD Simple where
    code = Simple . pack frontPadding id . digits

    decode (Simple b) = number $ unpack id b
