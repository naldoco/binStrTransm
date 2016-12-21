module Main where

import Test.Tasty
import Test.Tasty.HUnit

import BinStrTransm

message = "Hello world!"
b2i_1   = [1, 0, 1, 1]

binStrTransmSuite :: TestTree
binStrTransmSuite =
  testGroup "BinStrTransm tests"
    [ testGroup "Transmit"
        [ testCase ("Message: " ++ (show message) ++ " -> Hello world!") $
            (transmit message) @?= "Hello world!"
        ]
    , testGroup "bin2int"
      [ testCase ("bin2int " ++ (show b2i_1) ++ " -> 13") $
          (show $  bin2int            b2i_1) @?=    "13"
      ]
    ]

main = defaultMain binStrTransmSuite
