primes :: [Integer]
primes = sieve [2 ..] where sieve l = if null l then [] else p : sieve [x | x <- tail l, x `mod` p /= 0] where p = head l

primesBelow :: Integer -> [Integer]
primesBelow n = helper primes where helper l = if h >= n then [] else h : helper (tail l) where h = head l

primeDivisors :: Integer -> [Integer]
primeDivisors n = filter (\p -> n `mod` p == 0) (primesBelow n)

fourDivisors :: [Integer]
fourDivisors = filter (\n -> length (primeDivisors n) == 4) [1 ..]

main :: IO ()
main =
  print $ head $ map (fourDivisors !!) $ filter (\i -> fourDivisors !! (i + 3) == 3 + fourDivisors !! i) [1 ..]
