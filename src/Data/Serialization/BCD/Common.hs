module Data.Serialization.BCD.Common where

import           Data.Bits
import qualified Data.ByteString as BS
import           Data.List       (unfoldr)
import           Data.List.Extra (upper)
import           Data.Tuple      (swap)
import           Data.Word       (Word8)
import           Numeric         (readHex, showHex)

type Bytes = BS.ByteString

class SimpleHex a where
    toHex :: a -> String
    fromHex :: String -> a

class BCD a where
    code :: (Integral i, Bits i) => i -> a
    decode :: (Integral i, Bits i) => a -> i

instance SimpleHex Int where
    toHex x = upper $ showHex x ""
    fromHex x = let [(out, _)] = readHex x in out

pack :: [Word8] -> Bytes
pack = BS.pack . compress where
    compress :: [Word8] -> [Word8]
    compress xs | length xs `mod` 2 == 1 =
                    compress (0:xs)
                | otherwise = map pairToByte $ window xs

unpack :: Bytes -> [Word8]
unpack = decompress . BS.unpack where
    decompress :: [Word8] -> [Word8]
    decompress = concatMap byteToPair

sliding :: Int -> [a] -> [[a]]
sliding n = unfoldr (\xs -> let (a, as) = splitAt n xs in if null a then Nothing else Just (a, as))

window :: [a] -> [[a]]
window = sliding 2

digits :: Integral a => a -> [Word8]
digits = map fromIntegral . reverse . unfoldr decompose . abs where
    decompose x = let ab = x `quotRem` 10 in
        if ab == (0,0) then Nothing else Just $ swap ab

number :: (Integral a) => [Word8] -> a
number = foldl (\acc x -> acc * 10 + fromIntegral x) 0

swapPair :: [a] -> [a]
swapPair [a, b] = [b, a]
swapPair _      = error "Only pairs allowed"

pairToByte :: [Word8] -> Word8
pairToByte [a, b] = shiftL a 4 .|. b
pairToByte _      =      error "Only pairs allowed"

byteToPair :: Word8 -> [Word8]
byteToPair x = [shiftR x 4, x .&. 15]
