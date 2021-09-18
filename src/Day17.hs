{-| Given a list of containers (of various sizes)
    a) find the number of combinations that sum up to 150
    b) find the number of combinations (using minimal number of containers) that sum up to 150
-}
module Day17 where

import Prelude hiding (lookup)
import Data.Vector (generate, (!), (!?))
import Data.IntMap (IntMap, toList, fromList, keys, lookup, alter, empty, foldMapWithKey, fromListWith)
import qualified Data.IntMap as M ((!))
import Data.List (subsequences)
import qualified Data.Set as S
import Data.Set (Set)
import Data.Monoid (Product(Product, getProduct))
import Puzzle (Puzzle)

part1 :: Puzzle
part1 = show . solve1 150 . parseBottles . map read . lines

part2 :: Puzzle
part2 = show . solve2 150 . parseBottles . map read . lines

type Bottles = IntMap Int

parseBottles :: [Int] -> Bottles
parseBottles = fromListWith (+) . (`zip` repeat 1)

bottles :: Bottles
bottles = fromList [(20, 1), (15, 1), (10, 1), (5, 2)]

solve1 n bs = sum (map (options bs) (S.toList $ combinations bs n))

solve2 n bs = let combs = combinations bs n
                  m = minimum (S.map sum combs)
                  mins = S.filter ((==m) . sum) combs
              in sum (map (options bs) (S.toList mins))

-- | Gets all combinations of containers that sum up to a given value
combinations :: Bottles -> Int -> Set (IntMap Int)
combinations bs n = memo ! n where
    memo = generate (n + 1) f
    f :: Int -> Set (IntMap Int)
    f 0 = S.singleton empty
    f i = S.unions (map (sumUp i) (keys bs))
    sumUp i container = S.filter validCombination
                      $ maybe S.empty (S.map (increment container)) (memo !? (i - container))
    increment  = alter (Just . maybe 1 (+1))
    validCombination m = all ($m) [not . null, (`isSubMapOf` bs)]

-- | Calculates the number of possible container combinations for a given set of container sizes
-- >>> options (fromList [(5,2),(10,1)]) (fromList [(5,1)])
--     2
-- >>> options (fromList [(5,2),(10,1)]) (fromList [(5,2)])
--     1
options :: Bottles -> IntMap Int -> Int
options bs = getProduct . foldMapWithKey (\size count -> Product (binom count (bs M.! size)))

a `isSubMapOf` b = all f (toList a) where
    f (k,v) = case lookup k b of { Nothing -> False; (Just i) -> v <= i }

-- | binomial coefficient `n over k`
binom k n = product [1+n-k..n] `div` product [1..k]

-- Brute force solutions
solve1' :: Int -> [Int] -> [[Int]]
solve1' n = filter ((==n) . sum) . subsequences

solve2' :: Int -> [Int] -> [[Int]]
solve2' n xs = let options = solve1' n xs
                   m = minimum (map length options)
               in filter ((==m) . length) options
