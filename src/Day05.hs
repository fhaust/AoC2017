

module Day05 where

import Data.List
import Debug.Trace

type Machine = ([Int], [Int])


parseInput :: String -> [Int]
parseInput = map read . lines

escaped (ls,x:rs) | x >= 0 = x > length rs
                  | x <  0 = abs x > length ls

rule1 (ls,x:rs) | x >= 0 = let (a,b) = splitAt x (x+1:rs) in (reverse a ++ ls, b)
                | x <  0 = let (a,b) = splitAt (abs x) ls in (b, reverse a ++ (x+1:rs))

run1 i = (+1) <$>  findIndex escaped (iterate rule1 ([],i))


rule2 (ls,x:rs) | x >= 0 = let (a,b) = splitAt x ((if x >= 3 then x-1 else x+1):rs) in (reverse a ++ ls, b)
                | x <  0 = let (a,b) = splitAt (abs x) ls in (b, reverse a ++ ((if x >= 3 then x-1 else x+1):rs))

run2 i = (+1) <$>  findIndex escaped (iterate rule2 ([],i))
