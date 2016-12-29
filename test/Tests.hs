module Main where

import Test.Tasty
import Test.Tasty.HUnit

import BinStrTransm

message = "Hello world!"
message2 = "higher-order functions are easy"
b2i_1   = [1, 0, 1, 1] ; i2b_1 = 13
b2i_2   = [0, 0, 0, 0] ; i2b_2 =  0
b2i_3   = [1, 0, 0, 0] ; i2b_3 =  1
b2i_4   = [0, 0, 0, 1] ; i2b_4 =  8
b2i_5   = [1, 0, 1, 0] ; i2b_5 =  5
abc     = "ABC"
abc_1 = [65,66,67]
abc_2 = [[1,0,0,0,0,0,1,0],[0,1,0,0,0,0,1,0],[1,1,0,0,0,0,1,0]]
abc_3 = [ 1,0,0,0,0,0,1,0 , 0,1,0,0,0,0,1,0 , 1,1,0,0,0,0,1,0 ]

binStrTransmSuite :: TestTree
binStrTransmSuite =
  testGroup "BinStrTransm tests"
    [ testGroup "Transmit"
        [ testCase ("Message: " ++ (show message) ++ " -> Hello world!") $
            (transmit message) @?= "Hello world!"
        ]
    , testGroup "bin2int"
        [ testCase ("bin2int  " ++ (show b2i_1) ++  " -> 13") $
            (show $  bin2int            b2i_1) @?=     "13"
        , testCase ("bin2int  " ++ (show b2i_2) ++ " ->  0") $
            (show $  bin2int            b2i_2) @?=      "0"
        , testCase ("bin2int  " ++ (show b2i_3) ++ " ->  1") $
            (show $  bin2int            b2i_3) @?=      "1"
        , testCase ("bin2int  " ++ (show b2i_4) ++ " ->  8") $
            (show $  bin2int            b2i_4) @?=      "8"
        ]
    , testGroup "int2bin"
        [ testCase ("int2bin " ++ (show i2b_1) ++  "-> [1,0,1,1]") $
            (show $  int2bin            i2b_1) @?=    "[1,0,1,1]"
        , testCase ("int2bin  " ++ (show i2b_2) ++ "-> []") $
            (show $  int2bin            i2b_2) @?=    "[]"
        , testCase ("int2bin  " ++ (show i2b_3) ++ "-> [1]") $
            (show $  int2bin            i2b_3) @?=    "[1]"
        , testCase ("int2bin  " ++ (show i2b_4) ++ "-> [0,0,0,1]") $
            (show $  int2bin            i2b_4) @?=    "[0,0,0,1]"
        , testCase ("int2bin  " ++ (show i2b_5) ++ "-> [1,0,1]") $
            (show $  int2bin            i2b_5) @?=    "[1,0,1]"
        ]
    , testGroup "make8"
        [ testCase ("make8 . int2bin " ++ (show i2b_1) ++  "-> [1,0,1,1,0,0,0,0]") $
            (show $  make8 . int2bin $          i2b_1) @?=    "[1,0,1,1,0,0,0,0]"
        , testCase ("make8 . int2bin  " ++ (show i2b_2) ++ "-> [0,0,0,0,0,0,0,0]") $
            (show $  make8 . int2bin $          i2b_2) @?=    "[0,0,0,0,0,0,0,0]"
        , testCase ("make8 . int2bin  " ++ (show i2b_3) ++ "-> [1,0,0,0,0,0,0,0]") $
            (show $  make8 . int2bin $          i2b_3) @?=    "[1,0,0,0,0,0,0,0]"
        , testCase ("make8 . int2bin  " ++ (show i2b_4) ++ "-> [0,0,0,1,0,0,0,0]") $
            (show $  make8 . int2bin $          i2b_4) @?=    "[0,0,0,1,0,0,0,0]"
        , testCase ("make8 . int2bin  " ++ (show i2b_5) ++ "-> [1,0,1,0,0,0,0,0]") $
            (show $  make8 . int2bin $          i2b_5) @?=    "[1,0,1,0,0,0,0,0]"
        ]
    , testGroup "encode"
        [ testCase ("encode " ++ (show abc)   ++  "-> " ++ show abc_3) $
            (show $  encode $          abc) @?=            show abc_3
        ]
    , testGroup "chop8"
        [ testCase ("chop8 "  ++ (show abc_3) ++  "-> " ++ show abc_2) $
            (show $  chop8 $           abc_3) @?=          show abc_2
        ]
    , testGroup "decode"
        [ testCase ("decode " ++ (show abc_3) ++  "-> " ++ show abc) $
            (show $  decode $          abc_3) @?=          show abc
        ]
    , testGroup "transmit"
        [ testCase ("transmit " ++ (show abc) ++  "-> " ++ show abc) $
            (show $  transmit $          abc) @?=          show abc
        , testCase ("transmit " ++ (show message2) ++  "-> " ++ show message2) $
            (show $  transmit $          message2) @?=          show message2
        ]
    ]

main = defaultMain binStrTransmSuite
