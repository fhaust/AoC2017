

module Day10 where

import Data.Char (ord,chr,toLower)
import Data.List.Split (chunksOf,splitOn)
import Data.Bits (xor)
import Data.Hex

input = "94,84,0,79,2,27,81,1,123,93,218,23,103,255,254,243"

seed = (0,[0..255])

step (ss, xs) l = (ss+1, xs')
  where
    (a,b) = splitAt l xs
    (c,d) = splitAt ((l + ss) `mod` length xs) (reverse a ++ b)
    xs'   = d ++ c


-- apply step with every number from the input
oneRound i s = foldl step s i

-- position is tracked as first element of the list,
-- this rotates the list into its correct position
shiftResult i rs (ss,xs) = (ss, b ++ a)
  where
    n = negate (rs * sum i + sum [1..ss-1]) `mod` length xs
    (a,b) = splitAt n xs

-- parse input, run one round and multiply the first two elements
run1 = a * b
  where
    input'      = map read . splitOn "," $ input
    (_,(a:b:_)) = shiftResult input' 1 $ oneRound input' seed


-- add "salt"
input2lengths i = map ord i ++ [17, 31, 73, 47, 23]

-- run n rounds of the hashing function
manyRounds n s i = iterate (oneRound i) s !! n

-- convert sparse hash to dense hash
densify = map go . chunksOf 16
  where
    go = foldl1 xor

-- parse input, run 64 rounds, densify and convert to hex ... 
run2 = hash
  where
    input' = input2lengths input
    sparse = shiftResult input' 64 $ manyRounds 64 seed input'
    dense  = densify (snd sparse)
    hash   = map toLower . hex . map chr $ dense


