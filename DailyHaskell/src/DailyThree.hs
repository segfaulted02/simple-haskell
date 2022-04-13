module DailyThree where

--function that takes in an element and checks a list if there is
--anything that does not match it, and removes it
--accepts an element (a) and a list ([a]) and returns a new list ([a])
removeAllExcept :: Eq a => a -> [a] -> [a]
removeAllExcept c [] = []
removeAllExcept c (x:xs)
    | c == x = x : removeAllExcept c xs
    | otherwise = removeAllExcept c xs

--function that takes in an element and checks a list the amount of
--occurences and returns the count of occurences
--accepts an element (a) and a list ([a]) and returns an integer (Int)
countOccurences :: Eq a => a -> [a] -> Int
countOccurences c [] = 0
countOccurences c (x:xs)
    | c == x = 1 + (countOccurences c xs)
    | otherwise = countOccurences c xs

--function that takes in two elements and checks a list if there is
--any element that matches the first element, and replaces all
--occurences of the first element with the second element 
--accepts two elements (a and a) and a list ([a]) and returns a new list ([a])
substitute :: Eq a => a -> a -> [a] -> [a]
substitute a b [] = []
substitute a b (x:xs)
    | a == x = b : substitute a b xs
    | otherwise = x : substitute a b xs