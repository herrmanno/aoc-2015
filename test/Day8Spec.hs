module Day8Spec (spec) where

import Test.Hspec
import Day8 (part1,part2)

spec = do
    describe "day 8" $ do
        let input = unlines [ "\"\""
                            , "\"abc\""
                            , "\"aaa\\\"aaa\""
                            , "\"\\x27\""
                            ]
        it "part 1" $ 
            part1 input `shouldBe` "12"

        it "part 2" $ 
            part2 input `shouldBe` "19"