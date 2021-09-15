module Main where

import System.Environment (getArgs)
import Puzzle ( Puzzle, runPuzzle )
import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5
import qualified Day6
import qualified Day7
import qualified Day8
import qualified Day9
import qualified Day10
import qualified Day11

main :: IO ()
main = do
    args <- map read <$> getArgs
    case args of
        [d] -> runDay d (\(a,b) -> [a,b])
        [d,1] -> runDay d ((:[]) . fst)
        [d,2] -> runDay d ((:[]) . snd)
        _ -> putStrLn "Usage: AoC2015 <day> [part]"

-- |Executes the puzzle(s) for a given day
runDay :: Int -> (forall a. (a,a) -> [a]) -> IO ()
runDay i select = case getPuzzle i of
    (Just p) -> do
        input <- getContents
        mapM_ (`runPuzzle` input) (select p)
    Nothing -> putStrLn $ "Invalid puzzle 'day " <> show i <> "'"

-- |All the puzzles for the whole event
puzzles :: [(Puzzle, Puzzle)]
puzzles =
        [ (Day1.part1, Day1.part2)
        , (Day2.part1, Day2.part2)
        , (Day3.part1, Day3.part2)
        , (Day4.part1, Day4.part2)
        , (Day5.part1, Day5.part2)
        , (Day6.part1, Day6.part2)
        , (Day7.part1, Day7.part2)
        , (Day8.part1, Day8.part2)
        , (Day9.part1, Day9.part2)
        , (Day10.part1, Day10.part2)
        , (Day11.part1, Day11.part2)
        ]

getPuzzle :: Int -> Maybe (Puzzle, Puzzle)
getPuzzle i
    | i < 1 = Nothing
    | i > length puzzles = Nothing
    | otherwise = Just $ puzzles !! (i - 1)
