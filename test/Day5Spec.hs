module Day5Spec (spec) where

import Test.Hspec
import Day5 (part1, part2)

spec = do
    describe "day 5" $ do
        it "part 1 (1)" $
            part1 "ugknbfddgicrmopn" `shouldBe` "1"
        it "part 1 (2)" $
            part1 "aaa" `shouldBe` "1"
        it "part 1 (3)" $
            part1 "jchzalrnumimnmhp" `shouldBe` "0"
        it "part 1 (4)" $
            part1 "haegwjzuvuyypxyu" `shouldBe` "0"
        it "part 1 (4)" $
            part1 "dvszwmarrgswjxmb" `shouldBe` "0"

        it "part 2 (1)" $
            part2 "qjhvhtzxzqqjkmpb" `shouldBe` "1"
        it "part 2 (2)" $
            part2 "xxyxx" `shouldBe` "1"
        it "part 2 (3)" $
            part2 "uurcxstgmygtbstg" `shouldBe` "0"
        it "part 2 (4)" $
            part2 "ieodomkazucvgmuy" `shouldBe` "0"