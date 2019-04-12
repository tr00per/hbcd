module Data.Serialization.BCD.Simple where

import           Data.Serialization.BCD.Common

newtype Simple = Simple Bytes deriving (Show, Eq)
