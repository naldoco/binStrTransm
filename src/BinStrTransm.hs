module BinStrTransm where

type Bit = Int

transmit :: String -> String
transmit xs = "Hello world!"

bin2int :: [Bit] -> Int
bin2int bits = sum [x * y | (x,y) <- zip bits $  iterate (*2) 1]
