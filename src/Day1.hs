{-|
Interpreting '(' and ')' as 'going down' and 'going up' in an elevator and
1) find the resulting floor
2) find the first time one enters a floor below level 0
-}
module Day1 where

import Data.Foldable (foldl')
import Puzzle ( Puzzle )

part1 :: Puzzle
part1 = show . foldl' f 0 where
    f n '(' = n + 1
    f n ')' = n - 1
    f n c = error $ "Bad character '" <> [c] <> "'"

part2 :: Puzzle
part2 = show . fst . foldl' f (0, 0) . zip [1..] where
    f acc@(pos,level) (p,c)
        | level < 0 = acc
        | otherwise = (p, if c == '(' then level + 1 else level - 1)
