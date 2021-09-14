{-# LANGUAGE ViewPatterns #-}
{-| Given a list of distances between cities find
    a) the route w/ minimal cost
    b) the route w/ maximum cost
-}
module Day9 where

import Data.List (nub, permutations)
import Data.Map (Map, fromList, (!))
import Puzzle (Puzzle)

part1 :: Puzzle
part1 = show . minimum . allRouteCosts

part2 :: Puzzle
part2 = show . maximum . allRouteCosts


type City = String

allRouteCosts :: String -> [Int]
allRouteCosts input = let
    connectionList = map parse (lines input)
    connections = buildConnections connectionList
    cities = collectCities connectionList
    perms = permutations cities
    routes = map buildRoute perms
    in map (routeCost connections) routes


collectCities :: Eq a => [(a,a,b)] -> [a]
collectCities = nub . concatMap (\(a,b,_) -> [a,b])

parse :: String -> (City,City,Int)
parse (words -> [a,_,b,_,c]) = (a, b, read c)
parse s = error $ "Bad input :" <> s

buildConnections :: Traversable t => t (City, City, Int) -> Map (City, City) Int
buildConnections = fromList . concatMap (\(a,b,i) -> [((a,b),i), ((b,a), i)])

buildRoute :: [City] -> [(City, City)]
buildRoute = zip <$> id <*> tail

routeCost :: Map (City, City) Int -> [(City, City)] -> Int
routeCost m = sum . map (m !)
