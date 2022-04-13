module DailyThreeSpec where

import Test.Hspec
import DailyThree

spec :: Spec
spec = do
    describe "removeAllExcept" $ do
        it "blah blah blah" $
            removeAllExcept 1 [1,2,3,4] `shouldBe` [1]
        it "blah blah blah" $
            removeAllExcept 2 [] `shouldBe` []
        it "blah blah blah" $
            removeAllExcept 'a' ['a','a','a','b','c','d'] `shouldBe` ['a','a','a']    
    describe "countOccurences" $ do
        it "blah blah blah" $
            countOccurences 1 [] `shouldBe` 0
        it "blah blah blah" $
            countOccurences 2 [2,3,4,5,2] `shouldBe` 2
        it "blah blah blah" $
            countOccurences 'a' ['a','b','c'] `shouldBe` 1
    describe "substitute" $ do
        it "blah blah blah" $
            substitute 1 2 [1,2,3,4] `shouldBe` [2,2,3,4]
        it "blah blah blah" $
            substitute 2 3 [] `shouldBe` []
        it "blah blah blah" $
            substitute 'a' 'b' ['a','b','a'] `shouldBe` ['b','b','b']