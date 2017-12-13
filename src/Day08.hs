

module Day08 where


import qualified Data.Map as M
import qualified Data.Vector as V

import Control.Monad



parseInput = map words . lines




step regs [n0, op, a, _, n1, cond, b] | check n1 cond (read b) = update n0 op (read a)
                                      | otherwise              = regs
  where

    check n ">"  x = M.findWithDefault 0 n regs >  x
    check n ">=" x = M.findWithDefault 0 n regs >= x
    check n "<"  x = M.findWithDefault 0 n regs <  x
    check n "<=" x = M.findWithDefault 0 n regs <= x
    check n "==" x = M.findWithDefault 0 n regs == x
    check n "!=" x = M.findWithDefault 0 n regs /= x

    update n "inc" x = M.alter (Just . maybe x (+x)) n regs
    update n "dec" x = M.alter (Just . maybe (-x) (\a -> a-x)) n regs



run i = (last maxes, maximum (tail maxes))
  where
    maxes = map (maximum . M.elems) $ scanl step M.empty i
