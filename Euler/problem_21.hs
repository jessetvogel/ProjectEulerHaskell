-- Divisors
divisors :: Integer -> [Integer]
divisors n = [d | d <- [1 .. (n - 1)], n `mod` d == 0]

-- Is amicable
isAmmicable :: Integer -> Bool
isAmmicable a = a /= b && sum (divisors b) == a where b = sum (divisors a)

-- Take the largest prime factor of 600851475143
main :: IO ()
main = print $ sum [n | n <- [1 .. 10000], isAmmicable n]
