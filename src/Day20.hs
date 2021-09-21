{-| Given infinite elfs {e1, e2, e2…} that deliver presents to each nth house, what is
    a) the smallest housenumber, that receives *input* presents, if each elf delivers 10
       presents to each house
    b) the smallest housenumber, that receives *input* presents, if each elf delivers 11
       presents, but stops after 50 houses
-}
module Day20 where

import Puzzle (Puzzle)
import Prelude hiding (filter)
import Math.NumberTheory.ArithmeticFunctions (divisors)
import Data.List (find)
import Data.Set (filter)

part1 :: Puzzle
part1 = show . maybe (-1) snd . solve1 . read

solve1 n = find ((>=n) . snd) $ zip [1..] (map ((*10) . sum . divisors) [(1::Int)..])

part2 :: Puzzle
part2 = show . maybe (-1) snd . solve2 . read

solve2 n = find ((>=n) . snd) $ zipWith f [1..] (map divisors [(1::Int)..]) where
    f n is = (n, 11 * sum (filter ((>=n) . (*50)) is))

