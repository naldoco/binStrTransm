module BinStrTransm where

type Bit = Int

transmit :: String -> String
transmit xs = "Hello world!"

bin2int :: [Bit] -> Int
bin2int bits = sum [w * b | (w,b) <- zip weights bits]
                  where weights = iterate (*2) 1

int2bin :: Int -> [Bit]
int2bin = undefined
