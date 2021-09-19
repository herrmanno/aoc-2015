module Day16Spec (spec) where

import Test.Hspec
import Day16 (part1, part2)
import Data.Functor ((<&>))

spec = do
    describe "day 16" $ do
        let input = readFile "input/16.txt"
        it "part 1" $
            (input <&> part1 <&> take 6) `shouldReturn` "Sue 40"

        it "part 2" $
            (input <&> part2 <&> take 7) `shouldReturn` "Sue 241"
