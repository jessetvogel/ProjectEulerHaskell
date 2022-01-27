isqrt :: Integer -> Integer
isqrt = floor . sqrt . fromIntegral

isPrime :: Integer -> Bool
isPrime n = (n > 1) && null [x | x <- [2 .. isqrt n], n `mod` x == 0]

diagonalNumbers :: [Integer]
diagonalNumbers = scanl (+) 1 (concatMap (replicate 4) [2, 4 ..])

totalPrimes :: [Integer]
totalPrimes = scanl1 (+) $ map (\p -> if isPrime p then 1 else 0) diagonalNumbers

ratios :: [Float]
ratios = [fromIntegral (totalPrimes !! n) / (fromIntegral n + 1) | n <- [0 ..]]

main :: IO ()
main = print $ head [1 + n `div` 2 | n <- [4, 8 ..], (ratios !! n) < 0.10]
