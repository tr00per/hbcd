{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Serialization.BCD (
    BCD,
    Coded (..),
    SimpleHex (..)
) where

import           Control.Arrow   ((***))
import           Data.Bits
import qualified Data.ByteString as BS
import           Data.List       (unfoldr)
import           Data.List.Extra (unsnoc, upper)
import           Data.Maybe      (fromMaybe)
import           Data.Tuple      (swap)
import           Data.Word
import           Numeric         (readHex, showHex)


type Bytes = BS.ByteString
newtype BCD = BCD Bytes deriving (Show, Eq)

class SimpleHex a where
    toHex :: a -> String
    fromHex :: String -> a

class Coded a where
    code :: a -> BCD
    decode :: BCD -> a

instance SimpleHex Int where
    toHex x = upper $ showHex x ""
    fromHex x = let [(out, _)] = readHex x in out

instance SimpleHex BCD where
    toHex (BCD bs) = upper $ BS.foldr showHex "" bs
    fromHex = pack . map fst . concatMap (readHex . (:[]))

instance (Integral a, Bits a) => Coded a where
    code n = pack $ conv n ++ sign where
        conv :: Integral a => a -> [Word8]
        conv = map fromIntegral . digits . abs
        sign :: [Word8]
        sign | n < 0  = [13]
             | n >= 0 = [12]
             | otherwise = [0]
        decompose x = let ab = x `quotRem` 10 in if ab == (0,0) then Nothing else Just $ swap ab
        digits x = reverse $ unfoldr decompose x

    decode b = combine $ compose *** sign $ fromMaybe ([], 0) $ unsnoc $ unpack b where
        sign :: Num a => Word8 -> a
        sign x = case x of
            10 -> 1
            11 -> -1
            12 -> 1
            13 -> -1
            14 -> 1
            15 -> 1
            _  -> 0
        combine :: Num a => (a, a) -> a
        combine (num, sgn)= sgn * num
        compose :: Num a => [Word8] -> a
        compose = foldl (\acc x -> acc * 10 + fromIntegral x) 0

pack :: [Word8] -> BCD
pack = BCD . BS.pack . compress where
    compress :: [Word8] -> [Word8]
    compress xs | length xs `mod` 2 == 1 =
                    compress (0:xs)
                | otherwise = map pairToByte $ window xs
    pairToByte :: [Word8] -> Word8
    pairToByte [a, b] =
        shiftL a 4 .|. b
    pairToByte _ =
        error "Only pairs allowed"

unpack :: BCD -> [Word8]
unpack (BCD bytes) = decompress . BS.unpack $ bytes where
    decompress :: [Word8] -> [Word8]
    decompress = concatMap byteToPair
    byteToPair :: Word8 -> [Word8]
    byteToPair x = [shiftR x 4, x .&. 15]

sliding :: Int -> [a] -> [[a]]
sliding n = unfoldr (\xs -> let (a, as) = splitAt n xs in if null a then Nothing else Just (a, as))

window :: [a] -> [[a]]
window = sliding 2
