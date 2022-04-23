module WeeklyHaskellOne where

--function that removes all instances of a character from a string
--accepts a character and a string then returns a string
removeChar :: Char -> String -> String
removeChar c [] = []
removeChar c (x:xs)
    | c == x = removeChar c xs
    | otherwise = x : removeChar c xs

--function that removes the whitespace from a string
--accepts a string then returns a string
removeWhitespace :: String -> String
removeWhitespace [] = []
removeWhitespace str = removeChar ' ' (removeChar '\t' (removeChar '\r' (removeChar '\n' str)))

--function that removes the punctuation from a string
--accepts a string then returns a string
removePunctuation :: String -> String
removePunctuation "\&" = "\&"
removePunctuation str = removeChar '.' (removeChar ',' (removeChar '[' (removeChar ']' 
    (removeChar '(' (removeChar ')' (removeChar '{' (removeChar '}' str)))))))

--function that converts each character in a string to its respective ASCII value
--into an list
--accepts a string and returns a list of integers
charsToAscii :: String -> [Int]
charsToAscii [] = []
charsToAscii (x:xs) = fromEnum (x :: Char) : charsToAscii xs

--function that converts a list of ASCII values to the respective character list (a string)
--accepts a list of integers then returns a list of characters (essentially a string)
asciiToChars :: [Int] -> [Char]
asciiToChars [] = []
asciiToChars (x:xs) = toEnum x : asciiToChars xs

--function that shifts each value in an integer list by a given number, from 0-127
--accepts an integer and a list of integers then returns a list of integers
shiftInts :: Int -> [Int] -> [Int]
shiftInts _ [] = []
shiftInts 0 [x] = [x]
shiftInts x (y:ys) = (x + y) `mod` 128: shiftInts x ys

--function that works as a Caesar Cipher, by shifting a string by a given value
--in the ASCII alphabet
--accepts an integer and a string then returns a string
shiftMessage :: Int -> String -> String
shiftMessage 0 [x] = [x]
shiftMessage _ [] = []
shiftMessage x y = asciiToChars (shiftInts x (charsToAscii y))