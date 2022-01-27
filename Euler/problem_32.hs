import Data.Char (ord)
import Data.List (sort)

pandigital :: (Integer, Integer, Integer) -> Bool
pandigital (a, b, c) = sort (show a ++ show b ++ show c) == "123456789"

isqrt :: Integer -> Integer
isqrt = floor . sqrt . fromIntegral

triples :: Integer -> [(Integer, Integer, Integer)]
triples n = [(n, k, n `div` k) | k <- [1 .. isqrt n], n `mod` k == 0]

main :: IO ()
main = print $ sum [n | n <- [1 .. 10000], any pandigital (triples n)]
