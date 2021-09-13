module Day4Spec (spec) where

import Test.Hspec
import Day4 (part1)

spec = do
    describe "day 4" $ do
        it "part 1 (1)" $
            part1 "abcdef" `shouldBe` "609043"
        it "part 1 (2)" $
            part1 "pqrstuv" `shouldBe` "1048970"