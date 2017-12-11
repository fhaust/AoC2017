
import Test.Tasty
import Test.Tasty.HUnit

import qualified Day01 as D01
import qualified Day02 as D02
import qualified Day03 as D03
import qualified Day04 as D04
import qualified Day05 as D05
import qualified Day06 as D06
import qualified Day10 as D10
import qualified Day11 as D11

main :: IO ()
main = defaultMain tests


tests = testGroup "Tests" [ day01
                          , day02
                          , day03
                          , day04
                          , day05
                          , day06
                          , day10
                          , day11
                          ]


day01 = testGroup "Day01" [part1,part2,stars]
  where
    part1 = testGroup "Part1" [ testCase input $ D01.run1 input @?= output | (input,output) <- tests1 ]
    tests1 = [("1122", 3), ("1111", 4), ("1234", 0), ("91212129", 9)]

    part2 = testGroup "Part2" [ testCase input $ D01.run2 input @?= output | (input,output) <- tests2 ]
    tests2 = [("1212", 6), ("1221", 0), ("123425", 4), ("123123", 12), ("12131415", 4)]

    stars = testGroup "Stars" [ testCase "Part1" $ D01.run1 D01.input @?= 1171
                              , testCase "Part2" $ D01.run2 D01.input @?= 1024
                              ]

day02 = testGroup "Day02" [part1, part2]
  where
    part1 = testCase "Part1" $ do
      i <- D02.parseInput <$> readFile "inputs/day02.txt"
      D02.run1 i @?= 30994
    part2 = testCase "Part2" $ do
      i <- D02.parseInput <$> readFile "inputs/day02.txt"
      D02.run2 i @?= 233

day03 = testGroup "Day03" [part1,part2]
  where
    part1 = testCase "Part1" $ D03.run1 289326 @?= 419
    -- part 2 doesn't finish when compiled ... runs fine in interpreter
    part2 = testCase "Part2" $ D03.run2 289326 @?= Just 295229

day04 = testGroup "Day04" [part1, part2]
  where
    part1 = testCase "Part1" $ do
      i <- D04.parseInput <$> readFile "inputs/day04.txt"
      D04.run1 i @?= 383
    part2 = testCase "Part2" $ do
      i <- D04.parseInput <$> readFile "inputs/day04.txt"
      D04.run2 i @?= 265

day05 = testGroup "Day05" [part1, part2]
  where
    part1 = testCase "Part1" $ do
      i <- D05.parseInput <$> readFile "inputs/day05.txt"
      D05.run D05.rule1 i @?= Just 374269
    part2 = testCase "Part2" $ do
      i <- D05.parseInput <$> readFile "inputs/day05.txt"
      D05.run D05.rule2 i @?= Just 27720699

day06 = testCase "Day06" $ D06.run @?= (5042,1086)

day10 = testGroup "Day10" [part1,part2]
  where
    part1 = testCase "Part1" $ D10.run1 @?= 23715
    part2 = testCase "Part2" $ D10.run2 @?= "541dc3180fd4b72881e39cf925a50253"

day11 = testCase "Day11" $ do
    i <- D11.parseInput <$> readFile "inputs/day11.txt"
    D11.run i @?= (720,1485)
