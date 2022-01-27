import Data.List (elemIndex)
import Euler.Util (isPrime, primeFactorization, primes)

a :: Integer -> Integer
a n = helper 1
  where
    helper 0 = 1
    helper x = 1 + helper ((10 * x + 1) `mod` n)

main :: IO ()
main = print $ fst $ head $ filter (\(n, k) -> k > 10 ^ 6) $ map (\n -> (n, a n)) $ filter (\n -> gcd 10 n == 1) [10 ^ 6 ..]
