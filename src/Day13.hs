{-| Find the best seating of `n` guests
   a) given a list of 'who is happy to sit next to each other'
   b) including yourself
-}
module Day13 where

import Data.List (nub, permutations)
import Data.Tuple (swap)
import Data.Map (Map, fromList, keys, (!))
import Puzzle (Puzzle)

part1 :: Puzzle
part1 s = let hm = happinessMap (lines s)
              names = nub (map fst (keys hm))
              perms = permutations names
              amounts = map (happiness hm) perms
          in show (maximum amounts)

part2 :: Puzzle
part2 s = let hm = happinessMap (lines s)
              names = "Me":nub (map fst (keys hm))
              perms = permutations names
              amounts = map (happiness hm) perms
          in show (maximum amounts)

happiness :: Map (String,String) Int -> [String] -> Int
happiness hm = sum . map h . (zip <$> id <*> (tail . cycle)) where
    h a@(_,"Me") = 0
    h a@("Me",_) = 0
    h a = hm ! a + hm ! swap a

happinessMap :: [String] -> Map (String,String) Int
happinessMap = fromList . map parse

parse s = case words s of
    [a,_,"gain",amount,_,_,_,_,_,_,b] -> ((a, init b), read amount)
    [a,_,"lose",amount,_,_,_,_,_,_,b] -> ((a, init b), negate (read amount))
    s -> error $ "Bad input: " <> unwords s
