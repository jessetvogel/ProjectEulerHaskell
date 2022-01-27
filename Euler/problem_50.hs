primes :: [Integer]
primes = sieve [2 ..] where sieve l = if null l then [] else p : sieve [x | x <- tail l, x `mod` p /= 0] where p = head l

isPrime :: Integer -> Bool
isPrime n = (n > 1) && null [x | x <- [2 .. isqrt n], n `mod` x == 0]

isqrt :: Integer -> Integer
isqrt = floor . sqrt . fromIntegral

consecutiveSumOfPrimes :: Int -> [Integer]
consecutiveSumOfPrimes l = [sum $ take l (drop n primes) | n <- [0 ..]]

listUntil :: Integer -> [Integer] -> [Integer]
listUntil n l = if h > n then [] else h : listUntil n (tail l) where h = head l

safeHead :: [Integer] -> Integer
safeHead [] = 0
safeHead l = head l

main :: IO ()
main = print $ maximum [ safeHead [p | p <- listUntil 1000000 (consecutiveSumOfPrimes l), isPrime p] | l <- [21..1000]]