module Day9Spec (spec) where

import Test.Hspec
import Day9 (part1,part2)

spec = do
    describe "day 9" $ do
        let input = unlines [ "London to Dublin = 464"
                            , "London to Belfast = 518"
                            , "Dublin to Belfast = 141"
                            ]
        it "part 1" $
            part1 input `shouldBe` "605"

        it "part 2" $
            part2 input `shouldBe` "982"
