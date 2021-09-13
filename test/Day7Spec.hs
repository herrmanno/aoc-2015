module Day7Spec (spec) where

import Test.Hspec
import Day7 (part1,part2)

spec = do
    describe "day 7" $ do
        it "part 1" $ do
            s <- readFile "input/7.txt"
            part1 s `shouldBe` "956"
        it "part 2" $ do
            s <- readFile "input/7.txt"
            part2 s `shouldBe` "40149"