module Day17Spec (spec) where

import Test.Hspec
import Day17 (solve1, solve2)
import Data.IntMap (fromList)

spec = do
    describe "day 17" $ do
        let input = fromList [(20, 1), (15, 1), (10, 1), (5, 2)]
        it "part 1" $
            solve1 25 input `shouldBe` 4

        it "part 2" $
            solve2 25 input `shouldBe` 3
