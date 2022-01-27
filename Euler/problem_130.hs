import Data.List (elemIndex)
import Euler.Util (isPrime, primeFactorization, primes)

a :: Integer -> Integer
a n = helper 1
  where
    helper 0 = 1
    helper x = 1 + helper ((10 * x + 1) `mod` n)

main :: IO ()
main = print $ sum $ take 25 [n | n <- [2 ..], gcd 10 n == 1, not (isPrime n), (n - 1) `mod` a n == 0]
