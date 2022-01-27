isPalindrome :: String -> Bool
isPalindrome [] = True
isPalindrome word = (head word == last word) && (null (tail word) || isPalindrome (init (tail word)))

toBinary :: Integer -> String
toBinary 0 = ""
toBinary n = toBinary (n `div` 2) ++ show (n `mod` 2)

main :: IO ()
main = print $ sum [n | n <- [1 .. 1000000], isPalindrome (show n) && isPalindrome (toBinary n)]
