module Data.Serialization.BCD.Telephony where

import           Data.Serialization.BCD.Common

newtype Telephony = Telephony Bytes deriving (Show, Eq)
