module Day15Spec (spec) where

import Test.Hspec
import Day15 (part1, part2)

spec = do
    describe "day 13" $ do
        let input = [ "Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8"
                    , "Cinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3"
                    ]
        it "part 1" $
            part1 (unlines input) `shouldBe` "62842880"

        it "part 2" $
            part2 (unlines input) `shouldBe` "57600000"
