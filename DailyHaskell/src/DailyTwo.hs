module DailyTwo where

-- this function takes a list and makes a new one of every 4th element
-- on each line, respectively, handles the empty case and cases with 1, 2, or 3
-- elements, which means the result is empty.
-- on the last line, pattern matches each element of a list size 4 or more
-- and returns every 4th element
every4th :: [Integer] -> [Integer]
every4th [] = []
every4th [_, _, _] = []
every4th[_, _] = []
every4th[_] = []
every4th ( _ : _ : _ : x : xs) = x : every4th xs

-- this function takes two float lists and returns the quotient of each element
-- first line handles the base case, also assumes lists are the same size,
-- which eliminates the need for more base cases
-- second line takes each list and returns the quotient of the first elements
-- and subsequently iterates through the rest of the lists
tupleDotQuotient :: [Int] -> [Int] -> Int
tupleDotQuotient [] [] = 0
tupleDotQuotient (x:xs) (y:ys) = (x `div` y) + tupleDotQuotient xs ys

-- this function takes the string and appends it to each element in the list
-- as seen on the first line, through pattern matching
appendEach :: String -> [String] -> [String]
appendEach x [] = []
appendEach x (y:ys) = (x ++ y) : appendEach x ys

-- this function takes a list and removes all duplicates. first line handles
-- the base case. second line and subsequent lines handle if `elem` returns
-- true or false and adds to a new list accordingly with removed duplicates
toSetList :: [Integer] -> [Integer]
toSetList [] = []
toSetList (x:xs) = 
    if (x `elem` xs)
        then toSetList xs
    else x : toSetList xs