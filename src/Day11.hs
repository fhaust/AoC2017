

module Day11 where

import Data.List.Split (splitOn)

step (x,y,z) dir = case dir of
                     "n"  -> (x  ,y+1,z-1)
                     "ne" -> (x+1,y  ,z-1)
                     "se" -> (x+1,y-1,z  )
                     "s"  -> (x  ,y-1,z+1)
                     "sw" -> (x-1,y  ,z+1)
                     "nw" -> (x-1,y+1,z  )

norm (x,y,z) = (abs x + abs y + abs z) `div` 2

parseInput = splitOn "," . init

run i = (norm e, m)
  where
    (m,e) = foldl go (0,(0,0,0)) i
    go (m,c) d = (max m (norm c'), c') where c' = step c d



