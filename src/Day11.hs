{-| find the next password iteration
   a) that fulfills some rules
   b) find the next password given a)'s output as input
-}
module Day11 where

import Data.List (nub)
import Data.Char (ord, chr)
import Puzzle (Puzzle)

part1 :: Puzzle
part1 = toString . head . dropWhile (not . check) . tail . iterate (increase 26) . toArray

part2 :: Puzzle
part2 = toString . head . dropWhile (not . check) . tail . iterate (increase 26) . toArray . part1

toNum :: Char -> Int
toNum = subtract (ord 'a') . ord

toArray :: String -> [Int]
toArray = reverse . map toNum

toString :: [Int] -> String
toString = reverse . map fromNum where
    fromNum = chr . (+ord 'a')

increase _ [] = []
increase m (x:xs) =
    let (remainder,x') = (x + 1) `divMod` m
    in x':(if remainder == 1 then increase m xs else xs)

check = and . mapM ($) [not . containsBadNums, containsDecreasingSequence 3, containsDoubles 2]

containsBadNums xs = any (`elem` xs) badNums where
    badNums = map toNum "oli"

containsDoubles n = (>= n) . length . nub . doubles where
    doubles [] = []
    doubles [_] = []
    doubles (a:b:xs) = if a == b then a:doubles xs else doubles (b:xs)

containsDecreasingSequence n xs
    | length xs < n = False
    | otherwise = decreasing (take n xs) || containsDecreasingSequence n (drop 1 xs)

decreasing [] = True
decreasing [_] = True
decreasing (x:y:xs) = x == y + 1 && decreasing (y:xs)
