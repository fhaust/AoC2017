


module Day12 where

import           Data.Graph (Graph,Vertex)
import qualified Data.Graph as G
import           Data.List.Split (splitOn)



parseInput :: String -> (Graph, Vertex -> (Int, Int, [Int]), Int -> Maybe Vertex)
parseInput = G.graphFromEdges . map go . lines
  where
    go l = (read n, read n, map read . splitOn "," $ ns)
      where [n,ns] = splitOn "<->" l


run g = (length . G.bcc $ g, length . G.components $ g)

