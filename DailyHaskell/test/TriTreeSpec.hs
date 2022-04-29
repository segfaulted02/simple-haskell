module TriTreeSpec where

import Test.Hspec
import TriTree

spec :: Spec
spec = do
    describe "search" $ do
        it "makes me sleep" $
            search 1 (NodeTwo 1 2 Empty Empty Empty) `shouldBe` True
        it "makes me sleep" $
            search 1 (NodeTwo 4 5 (NodeTwo 2 3 (NodeOne 1 Empty Empty Empty) Empty Empty) Empty Empty)  `shouldBe` True
        it "makes me sleep" $
            search 4 Empty `shouldBe` False
    describe "insert" $ do
        it "makes me sleep" $
            insert 1 (NodeTwo 1 2 Empty Empty Empty) `shouldBe` NodeTwo 1 2 Empty (NodeOne 1 Empty Empty Empty) Empty
        it "makes me sleep" $
            insert 1 (NodeTwo 4 5 (NodeTwo 2 3 (NodeOne 1 Empty Empty Empty) Empty Empty) Empty Empty)  `shouldBe` NodeTwo 4 5 (NodeTwo 2 3 (NodeTwo 1 1 Empty Empty Empty) Empty Empty) Empty Empty
        it "makes me sleep" $
            insert 4 Empty `shouldBe` NodeOne 4 Empty Empty Empty
    describe "insertList" $ do
        it "makes me sleep" $
            insertList [1..5] Empty `shouldBe` NodeTwo 4 5 (NodeTwo 2 3 (NodeOne 1 Empty Empty Empty) Empty Empty) Empty Empty
        it "makes me sleep" $
            insertList [1..5] (NodeTwo 4 5 Empty Empty Empty) `shouldBe` NodeTwo 4 5 (NodeTwo 2 3 (NodeOne 1 Empty Empty Empty) Empty Empty) (NodeOne 4 Empty Empty Empty) (NodeOne 5 Empty Empty Empty)
        it "makes me sleep" $
            insertList ([] :: [Int]) Empty `shouldBe` Empty
    describe "identical" $ do
        --the test case directly below worked correctly in GHCI, however Haskell is freaking
        --out and will not allow two Empty parameters.
        --it "makes me sleep" $
        --    identical Empty Empty `shouldBe` True
        it "makes me sleep" $
            identical (NodeOne 1 Empty Empty Empty) (NodeOne 2 Empty Empty Empty) `shouldBe` False
        it "makes me sleep" $
            identical (NodeTwo 4 5 Empty Empty Empty) (NodeTwo 4 5 Empty Empty Empty) `shouldBe` True
        it "makes me sleep" $
            identical (NodeOne 1 Empty Empty Empty) Empty `shouldBe` False
    describe "treeMap" $ do
        it "makes me sleep" $
            treeMap (+1) (NodeOne 1 Empty Empty Empty) `shouldBe` NodeOne 2 Empty Empty Empty
        it "makes me sleep" $
            treeMap (*2) (NodeTwo 4 5 (NodeTwo 2 3 (NodeOne 1 Empty Empty Empty) Empty Empty) Empty Empty) `shouldBe` NodeTwo 8 10 (NodeTwo 4 6 (NodeOne 2 Empty Empty Empty) Empty Empty) Empty Empty
        it "makes me sleep" $
            treeMap (/3) Empty `shouldBe` Empty
    describe "treeFoldPreOrder" $ do
        it "makes me sleep" $
            treeFoldPreOrder (+) 4 (NodeTwo 4 5 (NodeTwo 2 3 (NodeOne 1 Empty Empty Empty) Empty Empty) Empty Empty) `shouldBe` 19
        it "makes me sleep" $
            treeFoldPreOrder (*) 5 Empty `shouldBe` 5
        it "makes me sleep" $
            treeFoldPreOrder (/) 18 (NodeTwo 2 3 (NodeOne 1 Empty Empty Empty) Empty Empty) `shouldBe` 3.0
    describe "treeFoldInOrder" $ do
        it "makes me sleep" $
            treeFoldInOrder (+) 4 (NodeTwo 4 5 (NodeTwo 2 3 (NodeOne 1 Empty Empty Empty) Empty Empty) Empty Empty) `shouldBe` 19 
        it "makes me sleep" $
            treeFoldInOrder (*) 5 Empty `shouldBe` 5
        it "makes me sleep" $
            treeFoldInOrder (/) 18 (NodeTwo 2 3 (NodeOne 1 Empty Empty Empty) Empty Empty) `shouldBe` 3.0
    describe "treeFoldPreOrder" $ do
        it "makes me sleep" $
            treeFoldPostOrder (+) 4 (NodeTwo 4 5 (NodeTwo 2 3 (NodeOne 1 Empty Empty Empty) Empty Empty) Empty Empty) `shouldBe` 19
        it "makes me sleep" $
            treeFoldPostOrder (*) 5 Empty `shouldBe` 5
        it "makes me sleep" $
            treeFoldPostOrder (/) 18 (NodeTwo 2 3 (NodeOne 1 Empty Empty Empty) Empty Empty) `shouldBe` 3.0