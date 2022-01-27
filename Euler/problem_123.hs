import Euler.Util (primes)

{-
    First note that
        (p - 1)^n + (p + 1)^n mod p^2
            = (n * p - 1) * (-1)^(n + 1) + (n * p + 1) mod p^2
            = if even n then 2 else 2 * n * p
-}

r :: Integer -> Integer -> Integer
r n p = if even n then 2 else (2 * n * p) `mod` (p ^ 2)

main :: IO ()
main = do
  let ns = [1 ..]
  let rs = zipWith r ns primes
  let nrs = zip ns rs

  print $ fst $ head $ filter (\(n, r) -> r > 10 ^ 10) nrs
