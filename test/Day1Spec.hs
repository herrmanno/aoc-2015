module Day1Spec (spec) where

import Test.Hspec
import Day1 (part1, part2)

spec = do
    describe "day 1" $ do
        it "part 1 (1)" $
            part1 "(())" `shouldBe` "0"
        it "part 1 (2)" $
            part1 "(()(()(" `shouldBe` "3"
        it "part 1 (3)" $
            part1 ")())())" `shouldBe` "-3"

        it "part 2 (1)" $
            part2 ")" `shouldBe` "1"
        it "part 2 (2)" $
            part2 "()())" `shouldBe` "5"