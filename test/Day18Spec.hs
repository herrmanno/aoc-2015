module Day18Spec (spec) where

import Test.Hspec
import Day18 (parse, solve1, solve2)

spec = do
    describe "day 18" $ do
        let input = unlines [ ".#.#.#"
                            , "...##."
                            , "#....#"
                            , "..#..."
                            , "#.#..#"
                            , "####.."
                            ]

        it "part 1 (solve1)" $
            solve1 4 (parse input) `shouldBe` 4

        it "part 2 (solve2)" $
            solve2 5 6 (parse input) `shouldBe` 17
