{-| Sum all numbers
    a) in a string of JSON
    b) in a string of JSON, excluding objects that have a propertiy w/ value 'red'
-}
module Day12 where

import Data.ByteString.Lazy.Char8 as B (pack)
import Data.Aeson (decode, Value(..))
import Data.Maybe (fromJust)
import Data.HashMap.Strict (elems)
import Data.Vector (toList)
import Puzzle (Puzzle)

part1 :: Puzzle
part1 = show . sum . countNums

part2 :: Puzzle
part2 = show . sum . count . fromJust . decode . B.pack

countNums :: String -> [Int]
countNums "" = []
countNums s = case (reads @Int) s of
                [(n,s')] -> n:countNums s'
                _ -> countNums (drop 1 s)

count :: Value -> [Int]
count (Object m)
    | String "red" `elem` elems m = []
    | otherwise = concatMap count (elems m)
count (Array vector) = concat $ toList (fmap count vector)
count (Number n) = [truncate n]
count _ = []
