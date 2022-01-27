-- Prime factors
primeFactors :: Integer -> [Integer]
primeFactors 1 = []
primeFactors n
    | null factors = [n]
    | otherwise = factors ++ primeFactors (n `div` head factors)
    where factors = take 1 (filter (\x -> (n `mod` x) == 0) [2..n - 1])

-- Take the largest prime factor of 600851475143
main::IO()
main = print $ maximum (primeFactors 600851475143)
