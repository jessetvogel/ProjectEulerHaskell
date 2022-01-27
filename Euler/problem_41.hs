import Data.List (sort)

isPandigital :: Integer -> Bool
isPandigital n = sort s == take (length s) "123456789" where s = show n

isqrt :: Integer -> Integer
isqrt = floor . sqrt . fromIntegral

isPrime :: Integer -> Bool
isPrime n = (n > 1) && null [x | x <- [2 .. isqrt n], n `mod` x == 0]

main :: IO ()
main = print $ maximum [n | n <- [1 .. 10000000], isPandigital n && isPrime n]
