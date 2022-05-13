module WeeklyThreeSpec where

import WeeklyThree
import Test.Hspec

spec :: Spec
spec = do
    describe "+" $ do
        it "tests" $
            (Vec[2,3]) + (Vec[2,3]) `shouldBe` Vec [4.0,6.0]
        it "tests" $
            (Vec[5,6]) + (Vec[1,2]) `shouldBe` Vec [6.0,8.0]
    describe "-" $ do
        it "tests" $
            (Vec[2,3]) - (Vec[2,3]) `shouldBe` Vec [0.0,0.0]
        it "tests" $
            (Vec[5,6]) - (Vec[1,2]) `shouldBe` Vec [4.0,4.0]
    describe "*" $ do
        it "tests" $
            (Vec[2,3]) * (Vec[2,3]) `shouldBe` Vec [4.0,9.0]
        it "tests" $
            (Vec[5,6]) * (Vec[1,2]) `shouldBe` Vec [5.0,12.0]
    describe "negate" $ do
        it "tests" $
            negate (Vec[2,3]) `shouldBe` Vec [-2.0,-3.0]
        it "tests" $
            negate (Vec[5,6]) `shouldBe` Vec [-5.0,-6.0]
    describe "abs" $ do
        it "tests" $
            abs (Vec[2,3]) `shouldBe` Vec [2.0,3.0]
        it "tests" $
            abs (Vec[-5,-6]) `shouldBe` Vec [5.0,6.0]
    describe "fromInteger" $ do
        it "tests" $
            fromInteger 5 `shouldBe` Vec [5.0]
        it "tests" $
            fromInteger 6 `shouldBe` Vec [6.0]
    describe "signum" $ do
        it "tests" $
            signum (Vec[0,3]) `shouldBe` Vec [0.0,1.0]
        it "tests" $
            signum (Vec[5,-6]) `shouldBe` Vec [1.0,-1.0]
    describe "==" $ do
        it "tests" $
            (Vec[2,3]) == (Vec[3,4]) `shouldBe` False
        it "tests" $
            (Vec[2,3]) == (Vec[2,3]) `shouldBe` True
    describe "/=" $ do
        it "tests" $
            (Vec[2,3]) /= (Vec[3,4]) `shouldBe` True
        it "tests" $
            (Vec[2,3]) /= (Vec[2,3]) `shouldBe` False
    describe "compare" $ do
        it "tests" $
            compare (Vec[2,3]) (Vec[3,4]) `shouldBe` LT
        it "tests" $
            compare (Vec[2,3]) (Vec[2,3]) `shouldBe` EQ
    describe "<" $ do
        it "tests" $
            (Vec[2,3]) < (Vec[3,4]) `shouldBe` True
        it "tests" $
            (Vec[2,3]) < (Vec[2,3]) `shouldBe` False
    describe "<=" $ do
        it "tests" $
            (Vec[2,3]) <= (Vec[3,4]) `shouldBe` True
        it "tests" $
            (Vec[2,3]) <= (Vec[2,3]) `shouldBe` True
    describe ">" $ do
        it "tests" $
            (Vec[2,3]) > (Vec[3,4]) `shouldBe` False
        it "tests" $
            (Vec[2,3]) > (Vec[2,3]) `shouldBe` False
    describe ">=" $ do
        it "tests" $
            (Vec[2,3]) >= (Vec[3,4]) `shouldBe` False
        it "tests" $
            (Vec[2,3]) >= (Vec[2,3]) `shouldBe` True
    describe "max" $ do
        it "tests" $
            max (Vec[2,3]) (Vec[3,4]) `shouldBe` Vec [3.0,4.0]
        it "tests" $
            max (Vec[2,3]) (Vec[2,3]) `shouldBe` Vec [2.0,3.0]
    describe "min" $ do
        it "tests" $
            min (Vec[2,3]) (Vec[3,4]) `shouldBe` Vec [2.0,3.0]
        it "tests" $
            min (Vec[2,3]) (Vec[1,2]) `shouldBe` Vec [1.0,2.0]
    describe "magnitude" $ do
        it "tests" $
            magnitude (Vec[2,3]) `shouldBe` 3.605551275463989
        it "tests" $
            magnitude (Vec[0,0]) `shouldBe` 0.0
    describe "<>" $ do
        it "tests" $
            (Vec[2,3]) <> (Vec[3,4]) `shouldBe` Vec [2.0,3.0,3.0,4.0]
        it "tests" $
            (Vec[0,0]) <> (Vec[1,1]) `shouldBe` Vec [0.0,0.0,1.0,1.0]
    describe "mempty" $ do
        it "tests" $
            mempty (Vec[2,3]) `shouldBe` ()
        it "tests" $
            mempty `shouldBe` ()
    describe "mappend" $ do
        it "tests" $
            mappend (Vec[2,3]) (Vec[3,4]) `shouldBe` Vec [2.0,3.0,3.0,4.0]
        it "tests" $
            mappend (Vec[0,0]) (Vec[1,1]) `shouldBe` Vec [0.0,0.0,1.0,1.0]
    describe "mconcat" $ do
        it "tests" $
            mconcat [(Vec[2,3]),(Vec[3,4])] `shouldBe` Vec [2.0,3.0,3.0,4.0]
        it "tests" $
            mconcat [(Vec[0,0]),(Vec[1,1])] `shouldBe` Vec [0.0,0.0,1.0,1.0]