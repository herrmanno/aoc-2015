module Day14Spec (spec) where

import Test.Hspec
import Day14 (solve1, solve2, parse)

spec = do
    describe "day 14" $ do
        let input = [ "Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds."
                    , "Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds."
                    ]
        it "part 1 (solve1 / distanceAfter)" $
            (solve1 1000 . map parse) input `shouldBe` 1120

        it "part 2 (solve2 / raceReindeers)" $
            (solve2 1000 . map parse) input `shouldBe` 689
