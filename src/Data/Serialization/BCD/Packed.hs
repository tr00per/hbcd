module Data.Serialization.BCD.Packed
    ( SimpleHex (..)
    , BCD (..)
    , Packed
    ) where

import           Data.Serialization.BCD.Common

import           Control.Arrow                 ((***))
import           Data.List.Extra               (unsnoc)
import           Data.Maybe                    (fromMaybe)
import           Data.Word                     (Word8)

newtype Packed = Packed Bytes deriving (Show, Eq)

instance SimpleHex Packed where
    toHex (Packed bs) = toHex bs
    fromHex = Packed . fromHex

instance BCD Packed where
    code n = Packed $ pack frontPadding id $ digits n ++ sign
        where
        sign :: [Word8]
        sign | n < 0  = [0xD]
             | n >= 0 = [0xC]
             | otherwise = [0]

    decode (Packed b) = combine $ number *** sign $ fromMaybe ([], 0) $ unsnoc $ unpack id b
        where
        sign :: Num a => Word8 -> a
        sign x = case x of
            0xA -> 1
            0xB -> -1
            0xC -> 1
            0xD -> -1
            0xE -> 1
            0xF -> 1
            _   -> 0
        combine :: Num a => (a, a) -> a
        combine (num, sgn)= sgn * num
