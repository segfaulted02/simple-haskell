module DailyEight where

--typeclass that defines the event
data Event = Event {name :: String, day :: Int, month :: String,
year :: Int, xlocation :: Float, ylocation :: Float} deriving (Eq, Show)

--function that checks whether an event happened within a certain year
--accepts an Int year and a list of events then returns a list of events
inYear :: Int -> [Event] -> [Event]
inYear _ [] = []
inYear yr lst = filter (\x -> (year x == yr)) lst

--function that determines whether a certain event happened within a certain day range
--accepts two Int days and a list of events then returns a list of strings containing
--the names of the events
inDayRange :: Int -> Int -> [Event] -> [String]
inDayRange _ _ [] = []
inDayRange start end lst = map (getName) (filter (\x -> (day x >= start && day x <= end)) lst)

--helper function for inDayRange that finds the name of an event
--accepts an event and returns a string of the name
getName :: Event -> String
getName ev = name ev

--function that checks if an event happened within a certain location and has the same name
--accepts a String name, 4 Float values of the location, and a list of events
--then returns a list of events that fit the criteria
inArea :: String -> Float -> Float -> Float -> Float -> [Event] -> [Event]
inArea _ _ _ _ _ [] = []
inArea str lx ux ly uy lst = filter (\x -> (str == name x) && 
    (lx <= xlocation x && ux >= xlocation x) && (ly <= ylocation x)) lst