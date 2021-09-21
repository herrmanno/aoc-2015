{-| Playing a classic RPG game
    a) what is the cheapest item combination to win
    b) what is the most expensive item combination to lose
-}
module Day21 where

import Puzzle (Puzzle)
import Data.Maybe (catMaybes)

part1 :: Puzzle
part1 = show . solve1 . parseBoss

solve1 boss = minimum [ sum (map cost its) | its <- buy, mkPlayer 100 its `defeats` boss ]

part2 :: Puzzle
part2 = show . solve2 . parseBoss

solve2 boss = maximum [sum (map cost its) | its <- buy, boss `defeats` mkPlayer 100 its ]

buy = do
    w <- Just <$> weapons
    a <- Nothing : map Just armors
    r1 <- Nothing : map Just rings
    r2 <- Nothing : filter (/=r1) (map Just rings)
    return $ catMaybes [w,a,r1,r2]

p1 `defeats` p2 = p1DeadRound > p2DeadRound where
    damage1To2 = max 1 (playerDamage p1 - playerArmor p2)
    damage2To1 = max 1 (playerDamage p2 - playerArmor p1)
    p1DeadRound = fromIntegral (hitPoints p1) / fromIntegral damage2To1
    p2DeadRound = fromIntegral (hitPoints p2) / fromIntegral damage1To2

data Item = Item { cost :: Int, damage :: Int, armor :: Int } deriving Eq
data Player = Player { hitPoints :: Int, playerDamage :: Int, playerArmor :: Int }

mkPlayer hp its = Player hp (sum (map damage its)) (sum (map armor its))

weapons = [ Item 8 4 0, Item 10 5 0, Item 25 6 0, Item 40 7 0, Item 74 8 0 ]
armors = [ Item 13 0 1, Item 31 0 2, Item 53 0 3, Item 75 0 4, Item 102 0 5]
rings = [ Item 25 1 0, Item 50 2 0, Item 100 3 0, Item 20 0 1, Item 40 0 2, Item 80 0 3 ]

parseBoss s = let [hp,d,a] = map (read . last . words) (lines s)
              in Player hp d a
