module DailySix where

import Test.Hspec
import DailySix

spec :: Spec
spec = do
    describe "shorterThan" $ do
        it "word" $
            shorterThan 5 ["words", "testing", "no"] `shouldBe` ["words","no"]
        it "word" $
            shorterThan 4 [] `shouldBe` []
        it "word" $
            shorterThan 1 ["big","words","are","hard"] `shouldBe` []
    describe "removeMultiples" $ do
        it "word" $
            removeMultiples 5 [10,5,9,8,7] `shouldBe` [9,8,7]
        it "word" $
            removeMultiples 4 [] `shouldBe` []
        it "word" $
            removeMultiples 1 [1,2,3,4,5,6,7,8,9] `shouldBe` []
    describe "onlyJust" $ do
        it "word" $
            onlyJust [Nothing, Just 5, Nothing, Just 10] `shouldBe` [Just 5,Just 10]
        it "word" $
            onlyJust [] `shouldBe` []
        it "word" $
            onlyJust [Nothing, Nothing, Nothing, Nothing] `shouldBe` []