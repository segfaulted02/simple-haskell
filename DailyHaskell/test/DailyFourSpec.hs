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
        it "random phrase" $
            zip3Lists [5,6][2,3][1,4] `shouldBe` [(5,2,1),(6,3,4)]
    describe "unzipTriples" $ do
        it "random phrase" $
            unzipTriples [(1,2,3),(4, 5, 6),(7, 8, 9)] `shouldBe` ([1,4,7],[2, 5, 8],[3, 6, 9])
        it "random phrase" $
            unzipTriples [('a','b','c'),('d','e','f'),('g','h','i')] `shouldBe` ("adg","beh","cfi")
        it "random phrase" $
            unzipTriples [(3,1,4),(5,5,5),(6,6,6)] `shouldBe` ([3,5,6],[1,5,6],[4,5,6])
        --attempted a test case of the empty list, and returns an odd "ambiguous type variable"
        --error. tried many methods to force haskell to accept it, to no avail (you can see an
        -- attempt below). however, GHCI returns the proper output of an empty list
        --it "random phrase" $
            --unzipTriples [] `shouldBe` ([],[],[])
            --(unzipTriples ([] :: [Int]) `shouldBe` (([],[],[]) :: [(Int),(Int),(Int)]))
    describe "mergeSorted3" $ do
        it "random phrase" $
            mergeSorted3 [2,3,5][1,8][-10,4,10] `shouldBe` [-1,0,1,2,3,4,5,8,10]
        it "random phrase" $
            mergeSorted3 [][2,3,5][] `shouldBe` [2,3,5]
        it "random phrase" $
            mergeSorted3 [1,2,3][][1,2,3] `shouldBe` [1,1,2,2,3,3]