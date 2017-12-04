module Day03 where

-- standard mod only works on integers
import Data.Fixed (mod')
import Data.List (find)

import qualified Data.IntMap.Lazy as M

-- | x or y positions of a discrete spiral
-- searched "space filling spiral", this came up: http://demonstrations.wolfram.com/DiscreteSpiral
spiral :: Int -> Int -> Int
spiral f t = ((-1) ^ i) * (f * (abs (i*i - t) - i) + i*i - t - i `mod` 2) `div` 2
  where
    i = round . sqrt . fromIntegral $ t

-- | 1d index to 2d address
i2a i = (spiral 1 (i-1), spiral (-1) (i-1))

-- | city block distance
cbd (x,y) = abs x + abs y

-- | city block distance of 1d index
run1 i = cbd $ i2a i

-- | 2d address to 1d index (with "cache")
a2i a = case lookup a a2iMap of (Just i) -> i
a2iMap = [ (i2a n, n) | n <- [1..] ]

-- | value of 1d index as per the rules
value :: Int -> Int
value = (map go [0..] !!)
  where
    go 0 = 0
    go 1 = 1
    go i = sum [ value i' | let (x,y) = i2a i, ox <- [-1,0,1], oy <- [-1,0,1], let i' = a2i (ox+x,oy+y), i' < i]

-- | first element with value over i
run2 i = find (> i) (map value [0..])



