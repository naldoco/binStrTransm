module Main where

import Test.Tasty
import Test.Tasty.HUnit

import BinStrTransm

message = "Hello world!"

binStrTransmSuite :: TestTree
binStrTransmSuite =
  testGroup "BinStrTransm tests"
    [ testGroup "Transmit"
      [ testCase ("Message: "++ (show message) ++ "-> Hello world!") $
          (transmit message) @?= "Hello world!"
      ]
    ]

main = defaultMain binStrTransmSuite
