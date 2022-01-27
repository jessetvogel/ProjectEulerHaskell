-- List of primes
primesUntil :: Integer -> [Integer]
primesUntil n = 2 : [p | p <- [3, 5 .. n], all (\d -> p `mod` d /= 0) [3, 5 .. (floor . sqrt . fromIntegral) p]]

-- Print the sum
main :: IO ()
main = print (sum (primesUntil 2000000))
