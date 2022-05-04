module DailySix where

--function that determines whether or not strings in a list are shorter than a number
--accepts a number and a list of strings then returns a list of strings
shorterThan :: Int -> [String] -> [String]
shorterThan n [] = []
shorterThan n str = filter (\x -> (length x <= n) == True) str

--function that removes all values that are multiples of a given number
--accepts a number and a list of numbers then returns a number
removeMultiples :: Int -> [Int] -> [Int]
removeMultiples n [] = []
removeMultiples n lst = filter (\x -> x `mod` n /= 0) lst

--function that removes Nothing values
--accepts a list of Maybes then returns a list of Maybes
onlyJust :: Eq a => [Maybe a] -> [Maybe a]
onlyJust [] = []
onlyJust lst = filter (\x -> x /= Nothing) lst