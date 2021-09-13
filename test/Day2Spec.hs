module Day2Spec (spec) where

import Test.Hspec
import Day2 (part1, part2)

spec = do
    describe "day 2" $ do
        it "part 1 (1)" $
            part1 "2x3x4" `shouldBe` "58"
        it "part 1 (2)" $
            part1 "1x1x10" `shouldBe` "43"

        it "part 2 (1)" $
            part2 "2x3x4" `shouldBe` "34"
        it "part 2 (2)" $
            part2 "1x1x10" `shouldBe` "14"