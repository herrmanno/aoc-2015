{-| Given a list of reindeers (having a speed, a duration (how long they run w/ speed)
    and a rest time (how long they *not move* after running) tell
    a) the max distance of the best after 2503 seconds
    b) how many seconds the best reindeer was in lead after 2503 seconds
-}
module Day14 where

import Data.List (maximumBy)
import Data.Function (on)
import Puzzle (Puzzle)

part1 :: Puzzle
part1 = show . solve1 2503 . map parse . lines

solve1 n = maximum . map (distanceAfter n )

part2 :: Puzzle
part2 = show . solve2 2503 . map parse . lines

solve2 n rs = let rss = map makeReindeerState rs
                  rsss = iterate raceReindeers (0, rss)
                  rss' = snd (rsss !! n)
              in maximum (map points rss')

-- Part 1
distanceAfter :: Int -> Reindeer -> Int
distanceAfter t (Reindeer speed duration rest) =
    let (phases,restTime) = t `divMod` (duration + rest)
    in phases * speed * duration + min restTime duration * speed

-- Part 2
data Reindeer = Reindeer { speed :: Int , duration :: Int , rest :: Int }

data ReindeerState = ReindeerState { reindeer :: Reindeer , distance :: Int , points :: Int }

makeReindeerState :: Reindeer -> ReindeerState
makeReindeerState r = ReindeerState r 0 0

score :: ReindeerState -> ReindeerState
score rs@ReindeerState { points } = rs { points = points + 1 }

moveReindeerAt :: Int -> ReindeerState -> ReindeerState
moveReindeerAt t rs@(ReindeerState r d p)
    | isMovingAt t r = rs { distance = distance rs + speed (reindeer rs) }
    | otherwise = rs

isMovingAt :: Int -> Reindeer -> Bool
isMovingAt t Reindeer { duration, rest } = let t' = t `rem` (duration + rest) in t' < duration

raceReindeers :: (Int,[ReindeerState]) -> (Int,[ReindeerState])
raceReindeers (t, rss) =
    let rss' = map (moveReindeerAt t) rss
        ahead = maximumBy (compare `on` distance) rss'
        scoreAhead rs
            | distance rs == distance ahead = score rs
            | otherwise = rs
    in (t + 1, map scoreAhead rss')

-- Part 1 & 2
parse :: String -> Reindeer
parse s = case words s of
            [_,_,_,s,_,_,d,_,_,_,_,_,_,r,_] -> Reindeer (read s) (read d) (read r)
            s -> error ("Bad input: " <> unwords s)
