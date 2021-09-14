{-| Play `n` turn of 'look-and-say' where
    a) n = 40
    b) n = 50
-}
module Day10 where

import Data.List (group)
import Puzzle (Puzzle)

part1 :: Puzzle
part1 = show . length  . (!!40) . iterate lookAndSay . map (read . (:[]))

part2 :: Puzzle
part2 = show . length  . (!!50) . iterate lookAndSay . map (read . (:[]))

lookAndSay :: [Int] -> [Int]
lookAndSay = concatMap (sequence (map ($) [length, head])) . group
