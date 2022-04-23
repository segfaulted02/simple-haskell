module WeeklyHaskellOneSpec where

import Test.Hspec
import WeeklyHaskellOne

spec :: Spec
spec = do
    describe "removeChar" $ do
        it "does something" $
            removeChar 'a' "abcde" `shouldBe` "bcde"
        it "does something" $
            removeChar 'y' "hello" `shouldBe` "hello"
        it "does something" $
            removeChar 'y' "" `shouldBe` ""
    describe "removeWhitespace" $ do
        it "does something" $
            removeWhitespace " h e l l o " `shouldBe` "hello"
        it "does something" $
            removeWhitespace "a     b       c  " `shouldBe` "abc"
        it "does something" $
            removeWhitespace "this is a string" `shouldBe` "thisisastring"
    describe "removePunctuation" $ do
        it "does something" $
            removePunctuation "h.e.l.l.o." `shouldBe` "hello"
        it "does something" $
            removePunctuation "(i {dont like} [to code])" `shouldBe` "i dont like to code"
        it "does something" $
            removePunctuation "" `shouldBe` ""
    describe "charsToAscii" $ do
        it "does something" $
            charsToAscii "hello" `shouldBe` [104,101,108,108,111]
        it "does something" $
            charsToAscii "test...ing" `shouldBe` [116,101,115,116,46,46,46,105,110,103]
        it "does something" $
            charsToAscii "" `shouldBe` []
    describe "asciiToChars" $ do
        it "does something" $
            asciiToChars [104,101,108,108,111] `shouldBe` "hello"
        it "does something" $
            asciiToChars [116,101,115,116,46,46,46,105,110,103] `shouldBe` "test...ing"
        it "does something" $
            asciiToChars [] `shouldBe` ""
    describe "shiftInts" $ do
        it "does something" $
            shiftInts 1 [2,4,6,127] `shouldBe` [3,5,7,0]
        it "does something" $
            shiftInts 128 [1,1,1,1,1] `shouldBe` [1,1,1,1,1]
        it "does something" $
            shiftInts 5 [] `shouldBe` []
    describe "shiftMessage" $ do
        it "does something" $
            shiftMessage 1 "hello" `shouldBe` "ifmmp"
        it "does something" $
            shiftMessage (-1) "ifmmp" `shouldBe` "hello"
        it "does something" $
            shiftMessage 10 "" `shouldBe` ""