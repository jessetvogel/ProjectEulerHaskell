import Data.Char

-- Number from 1 -- 100 to word
numberToWord :: Integer -> String
numberToWord n
    | n == 0 = "zero"
    | n == 1 = "one"
    | n == 2 = "two"
    | n == 3 = "three"
    | n == 4 = "four"
    | n == 5 = "five"
    | n == 6 = "six"
    | n == 7 = "seven"
    | n == 8 = "eight"
    | n == 9 = "nine"
    | n == 10 = "ten"
    | n == 11 = "eleven"
    | n == 12 = "twelve"
    | n == 13 = "thirteen"
    | n == 14 = "fourteen"
    | n == 15 = "fifteen"
    | n == 16 = "sixteen"
    | n == 17 = "seventeen"
    | n == 18 = "eighteen"
    | n == 19 = "nineteen"
    | n == 20 = "twenty"
    | n == 30 = "thirty"
    | n == 40 = "forty"
    | n == 50 = "fifty"
    | n == 60 = "sixty"
    | n == 70 = "seventy"
    | n == 80 = "eighty"
    | n == 90 = "ninety"
    | n == 1000 = "one thousand"
    | otherwise = if hundreds == 0 then numberToWord (tens * 10) ++ " " ++ numberToWord ones else if n == hundreds * 100 then numberToWord hundreds ++ " hundred" else numberToWord hundreds ++ " hundred and " ++ numberToWord (n - hundreds * 100)
    where ones = n `mod` 10
          tens = (n `div` 10) `mod` 10
          hundreds = (n `div` 100) `mod` 10

-- Print the number we are looking for
main :: IO ()
main = print $ sum $ map (length . filter isAlpha . numberToWord) [1..1000]