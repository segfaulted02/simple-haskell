module DailyTwoSpec where

import Test.Hspec
import DailyTwo

spec :: Spec
spec = do
    describe "every4th" $ do
        it "produces the every4th of [1,2,3,4]" $
            every4th [1,2,3,4] `shouldBe` [4]
        it "produces the every4th of []" $
            every4th [] `shouldBe` []
        it "produces the every4th of [1,2,3,4,5,6,7,8]" $
            every4th [1,2,3,4,5,6,7,8] `shouldBe` [4,8]
        it "produces the every4th of [1]" $
            every4th [1] `shouldBe` []    
    describe "tupleDotQuotient" $ do
        it "produces the tupleDotQuotient of [1,2] / [3,4]" $
            tupleDotQuotient [1,2] [3,4] `shouldBe` 5 `div` 6
        it "produces the tupleDotQuotient of [1,2,3] / [4,5,6]" $
            tupleDotQuotient [1,2,3] [4,5,6] `shouldBe` 23 `div` 20
        it "produces the tupleDotQuotient of [] / []" $
            tupleDotQuotient [] [] `shouldBe` 0
    describe "appendEach" $ do
        it "produces the appendEach for !!! and ['Hello', 'Goodbye']" $
            appendEach "!!!" ["Hello", "Goodbye"] `shouldBe` ["Hello!!!", "Goodbye!!!"]
        it "produces the appendEach for '' ['Test', 'Testing']" $
            appendEach "" ["Test", "Testing"] `shouldBe` ["Test", "Testing"]
        it "produces the appendEach for 'cookies' ['ilove', 'ihate']" $
            appendEach "cookies" ["ilove", "ihate"] `shouldBe` ["ilovecookies", "ihatecookies"]
    describe "toSetList" $ do
        it "produces the toSetList of [1,2,3,4,5]" $
            toSetList [1,2,3,4,5] `shouldBe` [1,2,3,4,5]
        it "produces the toSetList of [1,1,2,2,3,3]" $
            toSetList [1,1,2,2,3,3] `shouldBe` [1,2,3]
        it "produces the toSetList of []" $
            toSetList [] `shouldBe` []