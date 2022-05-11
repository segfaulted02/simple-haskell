module DailyEightSpec where

import Test.Hspec
import DailyEight

--Note, all of the test cases seem to not work due to "No instance for (Eq Event) arising from use of `shouldBe`"
--However, they work perfectly in GHCI, and the errors here are unknown in cause, as I have already declared empty lists as their necessary type

spec :: Spec
spec = do
    describe "inYear" $ do
        it "..." $
            inYear 2020 [(Event "chad" 15 "Apr" 2020 4.4 5.5), (Event "chad" 15 "Mar" 2021 5.5 4.4)] `shouldBe` [Event {name = "chad", day = 15, month = "Apr", year = 2020, xlocation = 4.4, ylocation = 5.5}]
        it "..." $
            inYear 2012 ([] :: [Event]) `shouldBe` ([] :: [Event])
        it "..." $
            inYear 2000 [(Event "chad" 15 "Apr" 2020 4.4 5.5), (Event "chad" 15 "Mar" 2021 5.5 4.4), (Event "class" 24 "Dec" 2012 3.3 8.8)] `shouldBe` []
    describe "inDayRange" $ do
        it "..." $
            inDayRange 6 19 [(Event "e1" 15 "Apr" 2020 4.4 5.5),(Event "e2" 29 "Mar" 2012 5.5 6.6),(Event "e3" 2 "Dec" 2002 6.6 7.7)] `shouldBe` ["e1"]
        it "..." $
            inDayRange 5 6 ([] :: [Event]) `shouldBe` ([] :: [String])
        it "..." $
            inDayRange 1 30 [(Event "e1" 15 "Apr" 2020 4.4 5.5),(Event "e2" 29 "Mar" 2012 5.5 6.6),(Event "e3" 2 "Dec" 2002 6.6 7.7)] `shouldBe` ["e1","e2","e3"]
    describe "inArea" $ do
        it "..." $
            inArea "e1" 4.4 5.5 6.6 7.7 ([] :: [Event]) `shouldBe` ([] :: [Event])
        it "..." $
            inArea "chad" 4.4 6.6 8.8 10.10 [(Event "chad" 15 "Mar" 2020 5.5 9.9),(Event "not" 20 "Apr" 2012 5.5 9.9),(Event "chad" 5 "Dec" 2002 1.1 11.1)] `shouldBe` [Event {name = "chad", day = 15, month = "Mar", year = 2020, xlocation = 5.5, ylocation = 9.9}]
        it "..." $
            inArea "chad" 4.4 6.6 8.8 10.10 [(Event "chad" 15 "Mar" 2020 5.5 9.9),(Event "chad" 20 "Apr" 2012 5.5 9.9),(Event "not" 5 "Dec" 2002 4.4 8.8)] `shouldBe` [Event {name = "chad", day = 15, month = "Mar", year = 2020, xlocation = 5.5, ylocation = 9.9},Event {name = "chad", day = 20, month = "Apr", year = 2012, xlocation = 5.5, ylocation = 9.9}]