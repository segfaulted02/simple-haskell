module DailyFive where
import Data.Char
import Test.Hspec (xdescribe)

--function that multiplies pairs of integers
--accepts a list of integer pairs then returns a list of integers
multPairs :: [(Int,Int)] -> [Int]
multPairs [] = []
multPairs lst = map (\(x,y) -> x*y) lst

--function that displays an integer and its square
--accepts a list of integers then returns a list of integer pairs containing the original
--integer and its square
squareList :: [Int] -> [(Int, Int)]
squareList [] = []
squareList lst = map (\x -> (x, x * x)) lst

--function that determines if a string starts with a lowercase letter
--accepts a list of strings and returns a list of booleans, representing true if
--the string begins with a lowercase letter, false otherwise
findLowercase :: [String] -> [Bool]
findLowercase [] = []
findLowercase lst = map getFirstLetter lst

--helper function for findLowercase to determine if first letter is lowercase
--accepts a string then returns a boolean
getFirstLetter :: String -> Bool
getFirstLetter "" = False
getFirstLetter (x:xs) = isLower x