import Euler.Util (primes)

-- To check whether p is a divisor of R(k),
-- we want to compute \sum_{i = 0}^{k - 1} 10^i `mod` p,
-- which is equal to (10^k - 1) / 9, and check whether it equals zero.
-- Equivalently, if 10^k = 1.

modPow :: Integer -> Integer -> Integer -> Integer
modPow b e 1 = 0
modPow b e m = modPow' b e m 1
  where
    modPow' b e 1 r = 0
    modPow' b 0 m r = r
    modPow' b e m r
      | e `mod` 2 == 1 = modPow' b' e' m (r * b `mod` m)
      | otherwise = modPow' b' e' m r
      where
        b' = b * b `mod` m; e' = e `div` 2

isFactorOfRk :: Integer -> Integer -> Bool
isFactorOfRk p k = modPow 10 k p == 1

main :: IO ()
main = print $ sum $ take 40 $ filter (`isFactorOfRk` (10 ^ 9)) $ drop 2 primes -- The test does not work for p = 2 and 3, so drop those two primes
-- main = print $ modPow 10 (10 ^ 9) 5
