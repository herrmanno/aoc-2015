module Day11Spec (spec) where

import Test.Hspec
import Day11 (part1)

spec = do
    describe "day 11" $ do
        it "part 1 (1)" $
            part1 "abcdefgh" `shouldBe` "abcdffaa"
        it "part 1 (2)" $
            part1 "ghijklmn" `shouldBe` "ghjaabcc"
