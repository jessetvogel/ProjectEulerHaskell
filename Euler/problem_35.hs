isqrt :: Integer -> Integer
isqrt = floor . sqrt . fromIntegral

isPrime :: Integer -> Bool
isPrime n = (n > 1) && null [x | x <- [2 .. isqrt n], n `mod` x == 0]

rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = zipWith const (drop n (cycle xs)) xs

isCirculurPrime :: Integer -> Bool
isCirculurPrime n = all isPrime [read (rotate k (show n)) | k <- [0 .. l - 1]] where l = length (show n)

main :: IO ()
main = print $ length [n | n <- [2 .. 1000000], isCirculurPrime n]
