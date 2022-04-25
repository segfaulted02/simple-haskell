module DailyFiveSpec where

import Test.Hspec
import DailyFive

spec :: Spec
spec = do
    describe "multPairs" $ do
        it "word" $
            multPairs [(1,2),(3,4),(5,6)] `shouldBe` [2,12,30]
        it "word" $
            multPairs [(1,1),(100,100),(4,4)] `shouldBe` [1,10000,16]
        it "word" $
            multPairs [] `shouldBe` []
    describe "squareList" $ do
        it "word" $
            squareList [1,3,2] `shouldBe` [(1,1),(3,9),(2,4)]
        it "word" $
            squareList [8,8,8] `shouldBe` [(8,64),(8,64),(8,64)]
        it "word" $
            squareList [] `shouldBe` []
    describe "findLowercase" $ do
        it "word" $
            findLowercase ["word","Word"] `shouldBe` [True, False]
        it "word" $
            findLowercase ["test", "test", "test"] `shouldBe` [True, True, True]
        it "word" $
            findLowercase [] `shouldBe` []