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

fib :: [Integer]
fib = 1 : 1 : [a + b | (a, b) <- zip fib (tail fib)]

p :: Integer
p = 1000000007

smallS :: Integer -> Integer
smallS n = (((n `mod` 9) + 1) * modPow 10 (n `div` 9) p - 1) `mod` p

bigS :: Integer -> Integer
bigS k
  | k `mod` 9 == 8 = let m = (k + 1) `div` 9 in (5 * (modPow 10 m p - 1) - 9 * m) `mod` p
  | otherwise = (bigS (k - k `mod` 9 - 1) + sum (map smallS [k - k `mod` 9 .. k])) `mod` p

main :: IO ()
main = do
  let fibs = drop 1 (take 90 fib)

  print $ sum (map bigS fibs) `mod` p

-- s(n) = ((n % 9) + 1) * 10^(n // 9) - 1
--
-- \sum_{n = 0}^{8} s(n) = \sum_{n = 0}^{8} ((n % 9) + 1) * 1 - 1 = 45 - 9 = 36
-- \sum_{n = 9}^{17} s(n) = \sum_{n = 9}^{17} ((n % 9) + 1) * 10 - 1 = 450 - 9 = 441
--
-- \sum_{n = 0}^{9 * m - 1} s(n) = 45 * (m ones = (10^(m + 1) - 1) / 9) - 9 * m
--                               = 5 * (10 ^ (m + 1) - 1) - 9 * m