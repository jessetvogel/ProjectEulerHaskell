-- List of primes
primes :: [Integer]
primes = sieve [2..] where sieve l = if null l then [] else p : sieve [x | x <- tail l, x `mod` p /= 0] where p = head l

-- Print the 10001'th prime number, that is, at index 10000
main :: IO ()
main = print (primes !! 10000)
