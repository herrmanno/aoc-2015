{-| Find 'nice' (e.g. not naughty) strings filtered by a set of rules
a) contain 3 vowels, no 'bad char' sequences and at least one double character (like 'aa')
b) contain a repeated sequence of two chars and a group like 'a_a'
-}
module Day5 where

import Puzzle (Puzzle)

part1 :: Puzzle
part1 = show . length . filter isValidString1 . lines

part2 :: Puzzle
part2 = show . length . filter isValidString2 . lines

data NaughtyStringResult1 = NaughtyStringResult1
    { vowels :: Int
    , doubleChar :: Bool
    , badChars :: Bool
    }

isValidString1 :: String -> Bool
isValidString1 s = valid $ go s (NaughtyStringResult1 0 False False) where
    go [] r = r
    go [c] r = if isVowel c then r {vowels = vowels r + 1 } else r
    go (a:b:xs) r = if isBadChars a b
        then r { badChars = True }
        else let r' = r
                    { vowels = vowels r + (if isVowel a then 1 else 0)
                    , doubleChar = doubleChar r || a == b
                    }
             in go (b:xs) r'
    valid r = not (badChars r) && vowels r >= 3 && doubleChar r

isValidString2 :: String -> Bool
isValidString2 s = containsChar s && containsPair s

containsChar [] = False
containsChar [_] = False
containsChar [_, _] = False
containsChar s@(a:b:c:_) = a == c || containsChar (drop 1 s)

containsPair [] = False
containsPair [c] = False
containsPair s = let (x,rest) = splitAt 2 s
                 in (rest `contains` x) || containsPair (drop 1 s)

contains [] _ = False
contains xs x = x == take (length x) xs || contains (drop 1 xs) x

isVowel 'a' = True
isVowel 'e' = True
isVowel 'i' = True
isVowel 'o' = True
isVowel 'u' = True
isVowel _ = False

isBadChars 'a' 'b' = True
isBadChars 'c' 'd' = True
isBadChars 'p' 'q' = True
isBadChars 'x' 'y' = True
isBadChars _ _ = False