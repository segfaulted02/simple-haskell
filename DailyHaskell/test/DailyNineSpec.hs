module DailyNineSpec where

import Test.Hspec
import DailyNine
import Data.Char

spec :: Spec
spec = do
    describe "firstFunctorLaw" $ do
        it "tests" $
            firstFunctorLaw (Just ('c', 35)) `shouldBe` True
        it "tests" $
            firstFunctorLaw [2,3,5,7,11] `shouldBe` True
    describe "secondFunctorLaw" $ do
        it "tests" $
            secondFunctorLaw isAlpha fst (Just ('c', 35)) `shouldBe` True
        it "tests" $
            secondFunctorLaw chr (+96) [2,3,5,7,11] `shouldBe` True
    describe "question 3 tests" $ do
        it "tests" $
            firstFunctorLaw ((Right Nothing) :: Either (Maybe Integer) (Maybe Integer)) `shouldBe` True
        it "tests" $
            firstFunctorLaw ((Right (Just 5)) :: Either (Maybe Integer) (Maybe Integer)) `shouldBe` True
        it "tests" $
            firstFunctorLaw ((Left "test") :: Either String (Maybe Integer)) `shouldBe` True
        it "tests" $
            secondFunctorLaw addMaybe subtractMaybe ((Right Nothing) :: Either (Maybe Integer) (Maybe Integer)) `shouldBe` True
        it "tests" $
            secondFunctorLaw addMaybe subtractMaybe ((Right (Just 5)) :: Either (Maybe Integer) (Maybe Integer)) `shouldBe` True
        it "tests" $
            secondFunctorLaw addMaybe subtractMaybe ((Left "test") :: Either String (Maybe Integer)) `shouldBe` True