module Day05 where

type Machine = ([Int], [Int])

parseInput :: String -> [Int]
parseInput = map read . lines

-- | move machine a given amount of steps forward or backward
move :: Int -> Machine -> Maybe Machine
move = go
  where
    go n (ls,rs) | n == 0                  = Just (ls,rs)
                 | n >  0 && not (null rs) = go (n-1) (head rs : ls, tail rs)
                 | n <  0 && not (null ls) = go (n+1) (tail ls, head ls : rs)
                 | otherwise               = Nothing

-- | rule 1: increase current position by one
rule1 (ls,r:rs) = move r (ls,(r+1):rs)

-- | rule 2: incrase current position based on value
rule2 (ls,r:rs) = move r (ls,(if r >= 3 then (r-1) else (r+1)):rs)

-- | run machine with given rule
run rule i = Just . length $ go ([],i)
  where
    go (_,[]) = []
    go s      = case rule s of
                  Nothing -> []
                  (Just s') -> s' : go s'
