{-| Divide a number of packages (integers) into n groups with same sum and find the minimal product
    of the minimal number of ints in on of those groups, where 
    a) n = 3
    b) n = 4
-}
module Day24 where

import Puzzle (Puzzle)

part1 :: Puzzle
part1 = show . solve 3 . map read . lines

part2 :: Puzzle
part2 = show . solve 4 . map read . lines

solve n ns = let target = sum ns `div` n
                 combs = chooseAll ns
                 grp = head $ dropWhile null $ map (filter ((==target) . sum)) combs
                 prods = map product grp
             in minimum prods

chooseAll xs = [ choose n xs | n <- [1..length xs] ]

choose _ [] = []
choose 1 xs = [ [x] | x <- xs ]
choose n (x:xs) = [ x:rest | rest <- choose (n - 1) xs ] ++ choose n xs
