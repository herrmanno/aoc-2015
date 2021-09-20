{-| Playing a kind of 'game of life'
    a) for 100 iterations
    b) for 100 iterations, where the corners are _fixed_
-}
module Day18 where

import Data.Map (Map, fromList, findWithDefault, mapWithKey, insert)
import Control.Arrow ((***))
import Puzzle (Puzzle)

part1 :: Puzzle
part1 = show . solve1 100 . parse

solve1 n = lightSum . (!!n) . iterate nextState

part2 :: Puzzle
part2 = show . solve2 100 100 . parse

solve2 n m = lightSum . (!!n) . iterate (withCorners m . nextState) . withCorners m

type Cell = (Int,Int)
type State = Map Cell Bool

lightSum :: State -> Int
lightSum = sum . fmap fromEnum

nextState :: State -> State
nextState s = mapWithKey f s where
    f pos True = neighboursCount pos `elem` [2,3]
    f pos False = neighboursCount pos == 3
    neighboursCount = length . filter id . neighbours s

withCorners :: Int -> State -> State
withCorners n m = foldr (uncurry insert) m (zip [(1,1), (1,n), (n,1), (n,n)] (repeat True))

neighbours :: State -> Cell -> [Bool]
neighbours s c = map (\c' -> findWithDefault False c' s) (neighbourPos c)

neighbourPos :: Cell -> [Cell]
neighbourPos p = map (($ p) . uncurry (***)) ops where
    ops = let ops' = [(+1), subtract 1, id] in init [(a,b) | a <- ops', b <- ops']

parse :: String -> State
parse s = fromList $ concatMap f (zip  [1..] (lines s)) where
    f (row,line) = zipWith (\col c -> ((row,col), (c == '#') || False)) [1..] line
