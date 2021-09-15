module Day12Spec (spec) where

import Test.Hspec
import Day12 (part1, part2)

spec = do
    describe "day 12" $ do
        it "part 1 (1)" $
            part1 "[1,2,3]" `shouldBe` "6"
        it "part 1 (2)" $
            part1 "{\"a\":{\"b\":4},\"c\":-1}" `shouldBe` "3"

        it "part 2 (1)" $
            part2 "[1,{\"c\":\"red\",\"b\":2},3]" `shouldBe` "4"
        it "part 2 (2)" $
            part2 "[1,\"red\",5]" `shouldBe` "6"
