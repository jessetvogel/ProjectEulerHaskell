quadratic :: Integer -> Integer -> Integer -> Integer
quadratic a b n = n * n + a * n + b

isPrime :: Integer -> Bool
isPrime n = (n > 1) && null [x | x <- [2 .. isqrt n], n `mod` x == 0]

isqrt :: Integer -> Integer
isqrt = floor . sqrt . fromIntegral

generatedPrimes :: Integer -> Integer -> [Integer]
generatedPrimes a b = helper a b 0 where helper a b n = if isPrime q then q : helper a b (n + 1) else [] where q = quadratic a b n

main :: IO ()
main = print $ snd $ maximum [(length (generatedPrimes a b), a * b) | a <- [-1000 .. 1000], b <- [-1000 .. 1000]]
