module BinStrTransm where

type Bit = Int

transmit :: String -> String
transmit xs = "Hello world!"

bin2int :: [Bit] -> Int
bin2int bits = sum [w * b | (w,b) <- zip weights bits]
                  where weights = iterate (*2) 1

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 = undefined
