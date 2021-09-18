{-| Given a set of properties (describing aunt sue)
   a) find the one aunt sue from a list that matches those properties
   b) if some properties aren't *hard*, but a range
-}
module Day16 where

import Data.Either (fromRight)
import Data.Map (fromList, Map, empty, findWithDefault)
import Text.Parsec
import Puzzle (Puzzle)
import Data.Char (isDigit)

part1 :: Puzzle
part1 = head . filter (findSue . parseSue) . lines

part2 :: Puzzle
part2 = head . filter (findSue2 . parseSue) . lines

theRealSue =
           [ ("children", 3)
           , ("cats", 7)
           , ("samoyeds", 2)
           , ("pomeranians", 3)
           , ("akitas", 0)
           , ("vizslas", 0)
           , ("goldfish", 5)
           , ("trees", 3)
           , ("cars", 2)
           , ("perfumes", 1)
           ]

type Sue = (Int, Map String Int)

findSue :: Sue -> Bool
findSue (i,m) = all match theRealSue where
    match (k,v) = findWithDefault v k m == v

findSue2 :: Sue -> Bool
findSue2 (i,m) = all match theRealSue where
    match (k@"cats",v) = findWithDefault (v + 1) k m > v
    match (k@"trees",v) = findWithDefault (v + 1) k m > v
    match (k@"pomeranians",v) = findWithDefault (v - 1) k m < v
    match (k@"goldfish",v) = findWithDefault (v - 1) k m < v
    match (k,v) = findWithDefault v k m == v

parseSue :: String -> Sue
parseSue s = fromRight (-1, empty) $ parse sue "" s where
    sue = (,) <$> header <*> properties
    header = string "Sue " *> (number <* string ": ")
    number = read <$> many1 (satisfy isDigit)
    properties = fromList <$> sepBy1 property (string ", ")
    property = (,) <$> many (satisfy (/=':')) <*> (string ": " *> number)
