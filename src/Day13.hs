

module Day13 where

import Data.List (findIndex)
import Data.List.Split (splitOn)

parseInput :: String -> [(Int,Int)]
parseInput = map go . lines
  where
    go x = let [o,l] = splitOn ": " x in (read o, read l)

-- position of scanner at time t
pos (o,l) t = abs ((t-l+1) `mod` (2 * (l-1)) - (l-1))

run1 i = sum [ o*l | (o,l) <- i, let c = pos (o,l) (-o), c == 0 ]

run2 i = findIndex (notElem 0) [ [ pos (o,l) (t+o) | (o,l) <- i] | t <- [0..] ]
