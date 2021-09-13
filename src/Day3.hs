{-|
Move santa on a map by ^,v,<,> instructions and count the places he went to
a) at least once
b) at least once, if the path is split by two santas, by taking only the even / odd
    instructions
-}
module Day3 where

import Puzzle (Puzzle)
import Data.Set (Set,fromList, size, union)
import Data.Bifunctor (bimap)
import Control.Arrow ((&&&))

part1 :: Puzzle
part1 = show . size . deliverPresents

part2 :: Puzzle
part2 = show . size . join . both deliverPresents . splitPath where
    splitPath = bimap (map snd) (map snd) . toEvenAndOdd . zip [0..]
    toEvenAndOdd = filter (even . fst) &&& filter (odd . fst)
    both = bimap <$> id <*> id
    join = uncurry union

deliverPresents :: String -> Set (Int,Int)
deliverPresents = fromList . scanl f (0,0) where
    f (x,y) '^' = (x, y + 1)
    f (x,y) 'v' = (x, y - 1)
    f (x,y) '<' = (x - 1, y)
    f (x,y) '>' = (x + 1, y)
    f _ c = error $ "Bad input '" <> [c] <> "'"
