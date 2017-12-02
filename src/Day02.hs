


module Day02 where

import Data.List (permutations, tails)
import Data.Maybe (mapMaybe)

parseInput :: String -> [[Int]]
parseInput = map (map read . words) . lines

run1 input = sum $ map (\xs -> abs (minimum xs - maximum xs)) input

run2 input = sum $ concatMap (concatMap go . tails) input
  where go [] = []
        go (x:xs) = mapMaybe (divResult x) xs

divResult x y | x `rem` y == 0 = Just (x `div` y)
              | y `rem` x == 0 = Just (y `div` x)
              | otherwise      = Nothing

