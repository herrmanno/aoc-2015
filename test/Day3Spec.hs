module Day3Spec (spec) where

import Test.Hspec
import Day3 (part1, part2)

spec = do
    describe "day 3" $ do
        it "part 1 (1)" $
            part1 ">" `shouldBe` "2"
        it "part 1 (2)" $
            part1 "^>v<" `shouldBe` "4"
        it "part 1 (3)" $
            part1 "^v^v^v^v^v" `shouldBe` "2"

        it "part 2 (1)" $
            part2 "^v" `shouldBe` "3"
        it "part 2 (2)" $
            part2 "^>v<" `shouldBe` "3"
        it "part 2 (3)" $
            part2 "^v^v^v^v^v" `shouldBe` "11"