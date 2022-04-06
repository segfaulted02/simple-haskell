module DailyOneSpec where

import Test.Hspec
import DailyOne

spec :: Spec
spec = do
    describe "quadratic" $ do
        it "produces the quadratic of 4, 3, 2, 1" $
            quadratic 4 3 2 1 `shouldBe` 9
        it "produces the quadratic of 10, 20, 30, 5" $
            quadratic 10 20 30 5 `shouldBe` 860
        it "produces the quadratic of 5.5, 4.7, 9.9, 3.2" $
            quadratic 5.5 4.7 9.9 3.2 `shouldBe` 121.916
    describe "scaleVector" $ do
        it "produces the scaled vector of 3 * <4, 5>" $
            scaleVector 3 (4, 5) `shouldBe` (12, 15)
        it "produces the scaled vector of 5 * <6, 9>" $
            scaleVector 5 (6, 9) `shouldBe` (30, 45)
        it "produces the scaled vector of 6 * <9, 8>" $
            scaleVector 6 (9, 8) `shouldBe` (54, 48)
    describe "tripleDistance" $ do
        it "produces the distance between <1, 2, 3> and <4, 5, 6>" $
            tripleDistance (1, 2, 3) (4, 5, 6) `shouldBe` 5.196
        it "produces the distance between <10, 10, 10> and <5, 5, 5>" $
            tripleDistance (10, 10, 10) (5, 5, 5) `shouldBe` 8.66
        it "produces the distance between <7, 4, 3> and <1, 6, 9>" $
            tripleDistance (7, 4, 3) (1, 6, 9) `shouldBe` 8.718