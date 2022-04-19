module DailyFourSpec where

import Test.Hspec
import DailyFour

spec :: Spec
spec = do
    describe "zip3Lists" $ do
        it "random phrase" $
            zip3Lists [1,2,3]['a','b','c'][4,5,6] `shouldBe` [(1,'a',4), (2,'b',5), (3,'c',6)]
        it "random phrase" $
            zip3Lists ['a','b','c']['d','e','f']['x','y','z'] `shouldBe` [('a','d','x'),('b','e','y'),('c','f','z')]
        --Note, due to an odd type error, the test below does not work, however, when inputted
        --into ghci, it works perfectly and returns the correct result.
        it "random phrase" $
            zip3Lists [][2,3][1,4] `shouldBe` []
    describe "unzipTriples" $ do
        it "random phrase" $
            unzipTriples [(1,2,3),(4, 5, 6),(7, 8, 9)] `shouldBe` ([1,4,7],[2, 5, 8],[3, 6, 9])
        it "random phrase" $
            unzipTriples [('a','b','c'),('d','e','f'),('g','h','i')] `shouldBe` ("adg","beh","cfi")
        --Note, due to an odd type error, the test below does not work, however, when inputted
        --into ghci, it works perfectly and returns the correct result.
        it "random phrase" $
            unzipTriples [] `shouldBe` ([], [], [])
    describe "mergeSorted3" $ do
        it "random phrase" $
            mergeSorted3 [2,3,5][1,8][-10,4,10] `shouldBe` [-1,0,1,2,3,4,5,8,10]
        it "random phrase" $
            mergeSorted3 [][2,3,5][] `shouldBe` [2,3,5]
        it "random phrase" $
            mergeSorted3 [1,2,3][][1,2,3] `shouldBe` [1,1,2,2,3,3]