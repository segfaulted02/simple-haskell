module DailySeven where

--function that merges a list of lists into a single list
--accepts a list of lists then returns a list
createOneList :: [[a]] -> [a]
createOneList [] = []
createOneList lst = foldr (++) [] lst

--function that finds the largest value in a list
--accepts a list of ints then returns an int
findLargest :: [Int] -> Int
findLargest lst = foldr (\x y -> if x > y then x else y) 0 lst

--function that checks if all values in a list are True
--accepts a list of bools then returns a bool
allTrue :: [Bool] -> Bool
allTrue [] = False
allTrue lst = foldr (&&) True lst