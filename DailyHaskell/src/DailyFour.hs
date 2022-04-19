module DailyFour where

--mimics the built-in zip function which takes three lists and 'zips' them together
--creating a new singular list. assuming lists are the same size, handles base cases
--and outputs a list of tuples, ie [1,3,5] [2,4,6] = [(1,2),(3,4),(5,6)]
zip3Lists :: [a] -> [b] -> [c] -> [(a,b,c)]
zip3Lists _ _ [] = []
zip3Lists _ [] _ = []
zip3Lists [] _ _ = []
zip3Lists (x:xs) (y:ys) (z:zs) = (x,y,z) : zip3Lists xs ys zs

--does the opposite of zip3Lists, and takes a list of tuples and returns a tuple of 3 lists
--since haskell does not give 3 different outputs. ie [(1,2),(3,4)] = ([1,3],[2,4])
unzipTriples :: [(a,b,c)] -> ([a], [b], [c])
unzipTriples [] = ([], [], [])
unzipTriples ((x, y, z):xs) = (x : first rem, y : second rem, z : third rem)
    where rem = unzipTriples xs
first :: (x,y,z) -> x
first (x, _, _) = x
second :: (x,y,z) -> y
second (_, y, _) = y
third :: (x,y,z) -> z
third (_, _, z) = z

--crazy funny function that essentially zips 3 lists then sorts them
--in this case, whilst zipping i also sort the elements
--accepts 3 lists and outputs a single list, sorted
--ie [2, 3, 5] [1, 8] [-1, 0, 4, 10] = [-1, 0, 1, 2, 3, 4, 5, 8, 10]
mergeSorted3 :: Ord a => [a] -> [a] -> [a] -> [a]
mergeSorted3 [] [] [] = []
-- --if two lists are blank and one list is filled
mergeSorted3 [] [] (x:xs) = x : mergeSorted3 [] [] xs
mergeSorted3 [] (x:xs) [] = x : mergeSorted3 [] xs []
mergeSorted3 (x:xs) [] [] = x : mergeSorted3 xs [] []
--if one list is blank and two lists are filled
mergeSorted3 [] (x:xs) (y:ys)
    | x >= y = y : mergeSorted3 [] (x:xs) ys
    | otherwise = x : mergeSorted3 [] xs (y:ys)
mergeSorted3 (x:xs) [] (y:ys)
    | x >= y = y : mergeSorted3 (x:xs) [] ys
    | otherwise = x : mergeSorted3 xs [] (y:ys)
mergeSorted3 (x:xs) (y:ys) []
    | x >= y = y : mergeSorted3 (x:xs) ys []
    | otherwise = x : mergeSorted3 xs (y:ys) []
--if all lists are filled
mergeSorted3 (x:xs) (y:ys) (z:zs)
    | x >= z && y >= z = z : mergeSorted3 (x:xs) (y:ys) zs
    | x >= y && z >= y = y : mergeSorted3 (x:xs) ys (z:zs)
    | y >= x && z >= x = x : mergeSorted3 xs (y:ys) (z:zs)