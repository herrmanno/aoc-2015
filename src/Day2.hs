{-|
Reading a list for present (box) dimensions and
1) calculate the needed amount of fabric
2) calculate the needed amount of ribbon
to wrap all the presents.
-}
module Day2 where

import Data.List.Split (splitOn)
import Data.List (sort)
import Puzzle (Puzzle)

parse :: String -> [[Int]]
parse = map f . lines where
    f = map read . splitOn "x"

part1 :: Puzzle
part1 = show . sum . map presentArea . parse where
    presentArea [w, h, l] = 2 * sum sides + minimum sides
        where sides = [w*h, w*l, h*l]
    presentArea s = error $ "Bad input '" <> show s <> "'"

part2 :: Puzzle
part2 = show . sum . map ribbonLength . parse where
    ribbonLength dims =
        let ribbon = 2 * sum (take 2 $ sort dims)
            bow = product dims
        in ribbon + bow
