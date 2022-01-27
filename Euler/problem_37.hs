isqrt :: Integer -> Integer
isqrt = floor . sqrt . fromIntegral

isPrime :: Integer -> Bool
isPrime n = (n > 1) && null [x | x <- [2 .. isqrt n], n `mod` x == 0]

isTrunctablePrime :: Integer -> Bool
isTrunctablePrime n = all isPrime (([read (take k s) | k <- [1 .. length s]]) ++ ([read (drop k s) | k <- [1 .. length s - 1]])) where s = show n

main :: IO ()
main = print $ sum [n | n <- [11 .. 1000000], isTrunctablePrime n]
