{-# LANGUAGE TypeSynonymInstances #-}
module Data.Serialization.BCD.Common where

import           Data.Bits
import qualified Data.ByteString as BS
import           Data.List       (unfoldr)
import           Data.List.Extra (upper)
import           Data.Tuple      (swap)
import           Data.Word       (Word8)
import           Numeric         (readHex, showHex)

type Bytes     = BS.ByteString
type Padding a = [a] -> [a]
type Swapping  = [Word8] -> [Word8]

class SimpleHex a where
    toHex :: a -> String
    fromHex :: String -> a

class BCD a where
    code :: (Integral i, Bits i) => i -> a
    decode :: (Integral i, Bits i) => a -> i

instance SimpleHex Bytes where
    toHex   = addPaddingIfOdd leadingZero . upper . BS.foldr showHex ""
    fromHex = unsafePack . map fst . concatMap (readHex . (:[]))

pack :: Padding Word8 -> Swapping -> [Word8] -> Bytes
pack padding swapping = BS.pack . compress . addPaddingIfOdd padding where
    compress :: [Word8] -> [Word8]
    compress = map (pairToByte . swapping) . window

unsafePack :: [Word8] -> Bytes
unsafePack = BS.pack . map pairToByte . window

unpack :: Swapping -> Bytes -> [Word8]
unpack swapping = decompress . BS.unpack where
    decompress :: [Word8] -> [Word8]
    decompress = concatMap (swapping . byteToPair)

sliding :: Int -> [a] -> [[a]]
sliding n = unfoldr (\xs -> let (a, as) = splitAt n xs in if null a then Nothing else Just (a, as))

window :: [a] -> [[a]]
window = sliding 2

digits :: Integral a => a -> [Word8]
digits = map fromIntegral . reverse . unfoldr decompose . abs where
    decompose x = let ab = x `quotRem` 10 in
        if ab == (0,0) then Nothing else Just $ swap ab

number :: (Integral a) => [Word8] -> a
number = foldl (\acc x -> acc * 10 + fromIntegral x) 0 . filter (< 10)

noSwap :: Swapping
noSwap = id

swapPair :: Swapping
swapPair [a, b] = [b, a]
swapPair _      = error "Only pairs allowed - must have even number of elements"

pairToByte :: [Word8] -> Word8
pairToByte [a, b] = shiftL a 4 .|. b
pairToByte _      =      error "Only pairs allowed - must have even number of elements"

byteToPair :: Word8 -> [Word8]
byteToPair x = [shiftR x 4, x .&. 15]

addPaddingIfOdd :: Padding a -> [a] -> [a]
addPaddingIfOdd padding xs
    | length xs `mod` 2 == 1 = padding xs
    | otherwise              = xs

frontPadding :: Padding Word8
frontPadding = (0:)

backPadding :: Padding Word8
backPadding = (++ [0xF])

leadingZero :: Padding Char
leadingZero = ('0':)
