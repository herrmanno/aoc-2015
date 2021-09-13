module Day6Spec (spec) where

import Test.Hspec
import Day6 (part1,part2)

spec = do
    describe "day 5" $ do
        it "part 1 (1)" $
            part1 "turn on 0,0 through 999,999" `shouldBe` "1000000"
        it "part 1 (2)" $
            part1 "toggle 0,0 through 999,0" `shouldBe` "1000"
        it "part 1 (3)" $
            part1 "turn on 499,499 through 500,500" `shouldBe` "4"

        it "part 2 (1)" $
            part2 "turn on 0,0 through 0,0" `shouldBe` "1"
        it "part 2 (2)" $
            part2 "toggle 0,0 through 999,999" `shouldBe` "2000000"