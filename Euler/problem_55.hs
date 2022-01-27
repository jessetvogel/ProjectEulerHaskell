reverseInt :: Integer -> Integer
reverseInt = read . reverse . show

isPalindromeInt :: Integer -> Bool
isPalindromeInt n = show n == reverse (show n)

isLychrel :: Integer -> Bool
isLychrel n = not $ any isPalindromeInt (take 50 seq) where seq = (n + reverseInt n) : [k + reverseInt k | k <- seq]

main :: IO ()
main = print $ length [n | n <- [1 .. 10000], isLychrel n]
