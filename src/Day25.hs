module Day25 where

import Puzzle (Puzzle)
import Data.List (unfoldr, tails)
import Data.Maybe (listToMaybe)

part1 :: Puzzle
part1 = show . (codes !!) . uncurry nthNumber . readCoord where
    readCoord = (\(a:b:_) -> (a,b)) . parseNumbers
    parseNumbers = unfoldr $ listToMaybe . concatMap reads . tails

part2 :: Puzzle
part2 = undefined

codes = 0 : iterate f 20151125 where
    f = (`rem` 33554393) . (*252533)

nthNumber :: Int -> Int -> Int
nthNumber row col = triang col + sum (take (row - 1) [col..]) where
    triang n = n * (n+1) `div` 2
