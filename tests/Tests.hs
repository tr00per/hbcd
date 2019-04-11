module Main where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck  as QC
import           Test.Tasty.SmallCheck  as SC

import           Data.Serialization.BCD

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests, properties]

properties :: TestTree
properties = testGroup "Properties" [scProps, qcProps]

scProps = testGroup "(checked by SmallCheck)"
  [ SC.testProperty "decode . code  == identity" $
      \number -> decode (code number) == (number::Int)
  ]

qcProps = testGroup "(checked by QuickCheck)"
  [ QC.testProperty "decode . code  == identity" $
      \number -> decode (code number) == (number::Int)
  ]

unitTests = testGroup "Unit tests"
  [ testCase "Positive identity" $
      code (1::Int) @?= (fromHex "1C" :: BCD)

  , testCase "Negative identity" $
      code (-1::Int) @?= (fromHex "1D" :: BCD)

  , testCase "Wide Positive identity" $
      code (1234567::Int) @?= (fromHex "1234567C" :: BCD)

  , testCase "Wide Negative identity" $
      code (-1234567::Int) @?= (fromHex "1234567D" :: BCD)

  , testCase "Wide Positive identity (as hex)" $
      toHex (code (1234567::Int)) @?= "1234567C"

  , testCase "Wide Negative identity (as hex)" $
      toHex (code (-1234567::Int)) @?= "1234567D"

  , testCase "Wide Positive identity (from hex)" $
      decode (fromHex "1234567C" :: BCD) @?= (1234567 :: Int)

  , testCase "Wide Negative identity (from hex)" $
      decode (fromHex "1234567D" :: BCD) @?= ((-1234567) :: Int)
  ]
