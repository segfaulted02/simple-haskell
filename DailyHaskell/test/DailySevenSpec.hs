module DailySevenSpec where

import Test.Hspec
import DailySeven
import Data.Bool (Bool(True))

spec :: Spec
spec = do
    describe "createOneList" $ do
        it "blank" $
            createOneList [[1,2],[3],[],[4,5]] `shouldBe` [1,2,3,4,5]
        it "blank" $
            createOneList ([] :: [[Int]]) `shouldBe` ([] :: [Int])
        it "blank" $
            createOneList [["test","test","test"],["list", "listy"],["words","words"]] `shouldBe` ["test","test","test","list","listy","words","words"]
    describe "findLargest" $ do
        it "blank" $
            findLargest [1,2,3,4,5,6] `shouldBe` 6
        it "blank" $
            findLargest [3,5,10,9,15] `shouldBe` 15
        it "blank" $
            findLargest [] `shouldBe` 0
    describe "allTrue" $ do
        it "blank" $
            allTrue [] `shouldBe` False
        it "blank" $
            allTrue [True,True,True,True] `shouldBe` True
        it "blank" $
            allTrue [True,False,True,True] `shouldBe` False