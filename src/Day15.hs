{-| Given a list of ingredients (having a sum of integer properties) find
   a) the best combinations of 100 ingredients
   b) which sum up to have 500 calories
-}
module Day15 where

import Data.List (unfoldr, tails, transpose)
import Data.Maybe (listToMaybe)
import Puzzle (Puzzle)

part1 :: Puzzle
part1 s = let ingredients = map (init . parse) (lines s)
              scores = map (score ingredients) (combinations (length ingredients) 100)
          in show $ maximum scores

part2 :: Puzzle
part2 s = let ingredients = map (init . parse) (lines s)
              calories = map (last . parse) (lines s)
              combs = (combinations (length ingredients) 100)
              combs500 = filter ((==500) . caloriesScore calories) combs
              scores = map (score ingredients) combs500
          in show $ maximum scores

combinations :: Int -> Int -> [[Int]]
combinations 0 s = []
combinations 1 s = [[s]]
combinations n s = [(i:xs) | i <- [0..s], xs <- combinations (n-1) (s-i) ]

type Ingridient = [Int]

score :: [Ingridient] -> [Int] -> Int
score is amounts = product $ map (max 0 . sum . zipWith (*) amounts) (transpose is)

caloriesScore :: [Int] -> [Int] -> Int
caloriesScore is amounts = sum $ zipWith (*) amounts is

parse :: String -> Ingridient
parse = unfoldr $ listToMaybe . concatMap reads . tails
