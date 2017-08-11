module Coverish.SummarySpec
    ( main
    , spec
    ) where

import SpecHelper
import Data.Ratio ((%))

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Summary" $ do
    describe "sTotal" $ do
        it "totals covered and missed" $ do
            let s = Summary 5 7 20

            sTotal s `shouldBe` 12

    describe "sPercent" $ do
        it "is the ratio of covered over total" $ do
            let s = Summary 5 10 0

            sPercent s `shouldBe` (1 % 3) -- 5/15

        it "handles 0 total lines" $ do
            let s = Summary 0 0 0

            sPercent s `shouldBe` 0

    describe "sStrength" $ do
        it "is the ration of hits to total" $ do
            let s = Summary 5 10 20

            sStrength s `shouldBe` (4 % 3) -- 20/15

        it "handles 0 total lines" $ do
            let s = Summary 0 0 0

            sStrength s `shouldBe` 0
