-- Date functions
data Day = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday deriving (Enum, Eq)

data Month = January | February | March | April | May | June | July | August | September | October | November | December deriving (Enum, Eq)

isLeapYear :: Int -> Bool
isLeapYear y = (y `mod` 4 == 0) && (y `mod` 100 /= 0 || y `mod` 400 == 0)

addToDay :: Day -> Int -> Day
addToDay d n = toEnum ((fromEnum d + n) `mod` 7)

daysInYear :: Int -> Int
daysInYear y = if isLeapYear y then 366 else 365

daysInMonth :: Month -> Int -> Int
daysInMonth January y = 31
daysInMonth February y = if isLeapYear y then 29 else 28
daysInMonth March y = 31
daysInMonth April y = 30
daysInMonth May y = 31
daysInMonth June y = 30
daysInMonth July y = 31
daysInMonth August y = 31
daysInMonth September y = 30
daysInMonth October y = 31
daysInMonth November y = 30
daysInMonth December y = 31

toDay :: Int -> Month -> Int -> Day
toDay 1 January 1900 = Monday
toDay d January 1900 = addToDay (toDay 1 January 1900) (d - 1)
toDay d January y = addToDay (toDay d January 1900) (sum [daysInYear x | x <- [1900 .. (y - 1)]])
toDay d m y = addToDay (toDay d January y) (sum [daysInMonth n y | n <- [January .. m]] - daysInMonth m y)

-- Print the number of Sundays blabla
main :: IO ()
-- main = print $ fromEnum $ toDay 3 May 1996
main = print $ length $ filter (== Sunday) [toDay 1 m y | y <- [1901 .. 2000], m <- [January .. December]]
