

module Day05 where

import Data.List
import Debug.Trace

type Machine = ([Int], [Int])


parseInput :: String -> [Int]
parseInput = map read . lines

escaped (ls,x:rs) | x >= 0 = x > length rs
                  | x <  0 = abs x > length ls

move :: Int -> Machine -> Maybe Machine
move = go
  where
    go n (ls,rs) | n == 0                  = Just (ls,rs)
                 | n >  0 && not (null rs) = go (n-1) (head rs : ls, tail rs)
                 | n <  0 && not (null ls) = go (n+1) (tail ls, head ls : rs)
                 | otherwise               = Nothing

rule1' (ls,r:rs) = move r (ls,(r+1):rs)

run1 = run rule1'


rule2' (ls,r:rs) = move r (ls,(if r >= 3 then (r-1) else (r+1)):rs)

run2 = run rule2'

-- | run with given rule
run rule i = Just . length $ go ([],i)
  where
    go (_,[]) = []
    go s      = case rule s of
                  Nothing -> []
                  (Just s') -> s' : go s'
