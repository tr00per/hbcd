module Main where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck            as QC
import           Test.Tasty.SmallCheck            as SC

import           Data.Serialization.BCD.Packed
import           Data.Serialization.BCD.Simple
import           Data.Serialization.BCD.Telephony

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests1, unitTests2, unitTests3, properties]

properties :: TestTree
properties = testGroup "Properties" [scProps, qcProps]

scProps :: TestTree
scProps = testGroup "Packed (by SmallCheck)"
    [ SC.testProperty "decode . code  == identity" $
      \number -> decode (code number :: Packed) == (number :: Int)

    , SC.testProperty "decode . code  == identity" $
      \number -> decode (code number :: Simple) == abs (number :: Int)

    ,  SC.testProperty "decode . code  == identity" $
      \number -> decode (code number :: Telephony) == abs (number :: Int)
    ]

qcProps :: TestTree
qcProps = testGroup "Packed (by QuickCheck)"
    [ QC.testProperty "decode . code  == identity" $
      \number -> decode (code number :: Packed) == (number :: Int)

    , QC.testProperty "decode . code  == identity" $
      \number -> decode (code number :: Simple) == abs (number :: Int)

    , QC.testProperty "decode . code  == identity" $
      \number -> decode (code number :: Telephony) == abs (number :: Int)
    ]

unitTests1 :: TestTree
unitTests1 = testGroup "Unit tests - Packed"
    [ testCase "Positive identity" $
      code (1::Int) @?= (fromHex "1C" :: Packed)

    , testCase "Negative identity" $
      code (-1::Int) @?= (fromHex "1D" :: Packed)

    , testCase "Wide Positive identity (odd)" $
      code (1234567::Int) @?= (fromHex "1234567C" :: Packed)

    , testCase "Wide Negative identity (odd)" $
      code (-1234567::Int) @?= (fromHex "1234567D" :: Packed)

    , testCase "Wide Positive identity (even)" $
      code (12345678::Int) @?= (fromHex "012345678C" :: Packed)

    , testCase "Wide Negative identity (even)" $
      code (-12345678::Int) @?= (fromHex "012345678D" :: Packed)

    , testCase "Wide Positive identity (as hex)" $
      toHex (code (1234567::Int) :: Packed) @?= "1234567C"

    , testCase "Wide Negative identity (as hex)" $
      toHex (code (-1234567::Int) :: Packed) @?= "1234567D"

    , testCase "Wide Positive identity (from hex)" $
      decode (fromHex "1234567C" :: Packed) @?= (1234567 :: Int)

    , testCase "Wide Negative identity (from hex)" $
      decode (fromHex "1234567D" :: Packed) @?= ((-1234567) :: Int)
    ]

unitTests2 :: TestTree
unitTests2 = testGroup "Unit tests - Simple"
    [ testCase "Positive identity" $
      code (1::Int) @?= (fromHex "01" :: Simple)

    , testCase "Negative identity" $
      code (-1::Int) @?= (fromHex "01" :: Simple)

    , testCase "Wide Positive identity (odd)" $
      code (1234567::Int) @?= (fromHex "01234567" :: Simple)

    , testCase "Wide Negative unidentity (odd)" $
      code (-1234567::Int) @?= (fromHex "01234567" :: Simple)

    , testCase "Wide Positive identity (even)" $
      code (12345678::Int) @?= (fromHex "12345678" :: Simple)

    , testCase "Wide Positive identity (as hex)" $
      toHex (code (1234567::Int) :: Simple) @?= "01234567"

    , testCase "Wide Negative unidentity (as hex)" $
      toHex (code (-1234567::Int) :: Simple) @?= "01234567"

    , testCase "Wide Positive identity (from hex, odd)" $
      decode (fromHex "01234567" :: Simple) @?= (1234567 :: Int)

    , testCase "Wide Positive identity (from hex, even)" $
      decode (fromHex "12345678" :: Simple) @?= (12345678 :: Int)
    ]

unitTests3 :: TestTree
unitTests3 = testGroup "Unit tests - Telephony"
    [ testCase "Positive identity" $
      code (1::Int) @?= (fromHex "1F" :: Telephony)

    , testCase "Negative identity" $
      code (-1::Int) @?= (fromHex "1F" :: Telephony)

    , testCase "Wide Positive identity (odd)" $
      code (1234567::Int) @?= (fromHex "2143657F" :: Telephony)

    , testCase "Wide Negative unidentity (odd)" $
      code (-1234567::Int) @?= (fromHex "2143657F" :: Telephony)

    , testCase "Wide Positive identity (even)" $
      code (12345678::Int) @?= (fromHex "21436587" :: Telephony)

    , testCase "Wide Positive identity (as hex)" $
      toHex (code (1234567::Int) :: Telephony) @?= "2143657F"

    , testCase "Wide Negative unidentity (as hex)" $
      toHex (code (-1234567::Int) :: Telephony) @?= "2143657F"

    , testCase "Wide Positive identity (from hex, odd)" $
      decode (fromHex "2143657F" :: Telephony) @?= (1234567 :: Int)

    , testCase "Wide Positive identity (from hex, even)" $
      decode (fromHex "21436587" :: Telephony) @?= (12345678 :: Int)
    ]
