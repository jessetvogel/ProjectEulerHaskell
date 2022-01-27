-- Check if a string is a palindrome
isPalindrome :: String -> Bool
isPalindrome [] = True
isPalindrome word = (head word == last word) && (null (tail word) || isPalindrome (init (tail word)))

-- List of possible products
products :: [Integer]
products = [a * b | a <- [100 .. 999], b <- [100 .. 999]]

-- Sublist of palindromic numbers
palindomicNumbers :: [Integer]
palindomicNumbers = filter (isPalindrome . show) products

-- Print the maximum palindromic number
main :: IO ()
main = print (maximum palindomicNumbers)
