module Day10Spec (spec) where

import Test.Hspec
import Day10 (lookAndSay)

spec = do
    describe "day 9" $ do
        let input = [1]
        let expected = [ [1,1]
                       , [2,1]
                       , [1,2,1,1]
                       , [1,1,1,2,2,1]
                       , [3,1,2,2,1,1]
                       ]
        it "example" $
            take 5 (drop 1 (iterate lookAndSay input)) == expected
