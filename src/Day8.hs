{-| read a list of strings and compare their lengths with
a) the number of encoded characters
b) their length after escaping
-}
module Day8 where

import Puzzle (Puzzle)

part1 :: Puzzle
part1 = show . sum . map lengthDiff . lines

part2 :: Puzzle
part2 = show . sum . map escapeLengthDiff . lines

lengthDiff = (-) <$> codeLength <*> (subtract 2 . charLength)

escapeLengthDiff = (-) <$> ((+2) . escapedCharLength) <*> codeLength

-- | length of string
codeLength = length

-- | length of 'interpreted' characters in string
charLength [] = 0
charLength xss@(x:xs)
    | take 2 xss == "\\x" = 1 + charLength (drop 4 xss)
    | x == '\\' = 1 + charLength (drop 2 xss)
    | otherwise = 1 + charLength xs

-- | length of string after escaping special characters
escapedCharLength [] = 0
escapedCharLength xss@(x:xs)
    | x == '\"' = 2 + escapedCharLength xs
    | x == '\\' = 2 + escapedCharLength xs
    |otherwise = 1 + escapedCharLength xs
