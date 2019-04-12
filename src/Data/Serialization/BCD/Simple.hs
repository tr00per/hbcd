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
    code = undefined

    decode = undefined
