

module Day04 where

import Data.List (nub, delete, elem, sort)

parseInput = map words . lines


condition1 l = nub l == l

run1 i = length . filter condition1 $ i

condition2 l = not (any (\w -> w `elem` delete w l') l') where l' = map sort l

run2 i = length . filter (\l -> condition1 l && condition2 l) $ i
