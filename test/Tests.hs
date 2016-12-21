module Main where

import Test.Tasty
import Test.Tasty.HUnit

import BinStrTransm

message = "Hello world!"
b2i_1   = [1, 0, 1, 1]
b2i_2   = [0, 0, 0, 0]
b2i_3   = [1, 0, 0, 0]
b2i_4   = [0, 0, 0, 1]

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
      , testCase ("bin2int " ++ (show b2i_2) ++ " ->  0") $
          (show $  bin2int            b2i_2) @?=     "0"
      , testCase ("bin2int " ++ (show b2i_3) ++ " ->  1") $
          (show $  bin2int            b2i_3) @?=     "1"
      , testCase ("bin2int " ++ (show b2i_4) ++ " ->  8") $
          (show $  bin2int            b2i_4) @?=     "8"      ]
    ]

main = defaultMain binStrTransmSuite
