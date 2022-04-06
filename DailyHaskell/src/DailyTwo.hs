module DailyTwo where

every4th :: [Integer] -> [Integer]
every4th list =
    if (length list == 0)
    then 0
    else if (length list >= 5)
    then head (take 4 list) + every4th list

tupleDotProduct :: [Integer] -> [Integer] -> Integer
tupleDotProduct one two =

appendEach :: String -> [String] -> [String]
appendEach str list =
    (head list ++ str) + appendEach tail str list