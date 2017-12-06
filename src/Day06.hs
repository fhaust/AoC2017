

module Day06 where

import Data.List
import Data.Maybe

input :: [Int]
input = [5, 1, 10, 0, 1, 7, 13, 14, 3, 12, 8, 10, 7, 12, 0, 6]



-- | find maximum and associated index 
-- (easy way out ... two list traversals)
maximumIndex l = (m, fromJust $ elemIndex m l) where m = maximum l

-- | spread the highest value over the memory locations
step l = zipWith (\i x -> x + q + extra i) [0..] l
  where
    len     = length l
    (mv,mi) = maximumIndex l

    -- maximum is distributed and a bit is left over
    (q,r)   = mv `divMod` len

    -- does this element need special attention?
    extra i | i == mi   = - mv   -- drop the maximum value
            | i' <= r   = 1      -- add leftovers
            | otherwise = 0
      where
        -- index relative to maximum
        i' = (i - mi) `mod` len


run = (part1 , part2 + 1)
  where
    -- iterate until the current element is found in the "set"
    (s,x) = until (\(s,x) -> x `elem` s) (\(s,x) -> (x : s, step x)) ([], input)
    -- steps until repetition
    part1 = length s
    -- steps from the repetition to its origin (off by one)
    (Just part2) = elemIndex x s


