module Main where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck         as QC
import           Test.Tasty.SmallCheck         as SC

import           Data.Serialization.BCD.Packed

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests, properties]

properties :: TestTree
properties = testGroup "Properties" [scProps, qcProps]

scProps :: TestTree
scProps = testGroup "(checked by SmallCheck)"
  [ SC.testProperty "decode . code  == identity" $
      \number -> decode (code number :: Packed) == (number :: Int)
  ]

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)"
  [ QC.testProperty "decode . code  == identity" $
      \number -> decode (code number :: Packed) == (number :: Int)
  ]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "Positive identity" $
      code (1::Int) @?= (fromHex "1C" :: Packed)

  , testCase "Negative identity" $
      code (-1::Int) @?= (fromHex "1D" :: Packed)

  , testCase "Wide Positive identity" $
      code (1234567::Int) @?= (fromHex "1234567C" :: Packed)

  , testCase "Wide Negative identity" $
      code (-1234567::Int) @?= (fromHex "1234567D" :: Packed)

  , testCase "Wide Positive identity (as hex)" $
      toHex (code (1234567::Int) :: Packed) @?= "1234567C"

  , testCase "Wide Negative identity (as hex)" $
      toHex (code (-1234567::Int) :: Packed) @?= "1234567D"

  , testCase "Wide Positive identity (from hex)" $
      decode (fromHex "1234567C" :: Packed) @?= (1234567 :: Int)

  , testCase "Wide Negative identity (from hex)" $
      decode (fromHex "1234567D" :: Packed) @?= ((-1234567) :: Int)
  ]
