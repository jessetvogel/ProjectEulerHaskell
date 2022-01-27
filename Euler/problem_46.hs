primes :: [Integer]
primes = sieve [2 ..] where sieve l = if null l then [] else p : sieve [x | x <- tail l, x `mod` p /= 0] where p = head l

primesBelow :: Integer -> [Integer]
primesBelow n = helper primes where helper l = if h >= n then [] else h : helper (tail l) where h = head l

isPrime :: Integer -> Bool
isPrime n = n `elem` primesBelow (n + 1)

isqrt :: Integer -> Integer
isqrt = floor . sqrt . fromIntegral

isSquare :: Integer -> Bool
isSquare n = n == r * r where r = isqrt n

counterexamples :: [Integer]
counterexamples =
  filter
    ( \n ->
        not (isPrime n)
          && all (\p -> not (isSquare ((n - p) `div` 2))) (primesBelow n)
    )
    [2 ..]

main :: IO ()
main = print $ head counterexamples
