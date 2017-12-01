
import Test.Tasty
import Test.Tasty.HUnit

import qualified Day01 as D01

main :: IO ()
main = defaultMain tests


tests = testGroup "Tests" [day01]


day01 = testGroup "Day01" [part1,part2,stars]
  where
    part1 = testGroup "Part1" [ testCase input $ D01.run1 input @?= output | (input,output) <- tests1 ]
    tests1 = [("1122", 3), ("1111", 4), ("1234", 0), ("91212129", 9)]

    part2 = testGroup "Part2" [ testCase input $ D01.run2 input @?= output | (input,output) <- tests2 ]
    tests2 = [("1212", 6), ("1221", 0), ("123425", 4), ("123123", 12), ("12131415", 4)]

    stars = testGroup "Stars" [ testCase "Part1" $ D01.run1 D01.input @?= 1171
                              , testCase "Part2" $ D01.run2 D01.input @?= 1024
                              ]


