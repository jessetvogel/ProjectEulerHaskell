-- n^3 + n^2 * p = (n + k)^3 = n^3 + 3 * k * n^2 + 3 * k^2 * n + k^3
-- n^2 * p = 3 * k * n^2 + 3 * k^2 * n + k^3
-- n^2 * (p - 3 * k) = 3 * n * k^2 + k^3
-- From which follows that p >= 3 * k, so k <= p `div` 3
-- Given a value of k, we can solve for n

import Control.Monad (when)
import Data.Foldable (for_)
import Euler.Util (isqrt, primes)

isCube :: Integer -> Bool
isCube n = round (fromIntegral n ** (1 / 3)) ^ 3 == n

isGoodP :: Integer -> Bool
isGoodP p =
  any
    ( \k ->
        let n = - (isqrt (k ^ 3 * (4 * p - 3 * k)) + 3 * k ^ 2) `div` (6 * k - 2 * p)
         in (n + k) ^ 3 == n ^ 3 + n ^ 2 * p
    )
    [max 1 ((p - 1) `div` 3 - 1000) .. (p - 1) `div` 3]

findK :: Integer -> Integer
findK p = head [k | k <- [1 .. (p - 1) `div` 3], let n = - (isqrt (k ^ 3 * (4 * p - 3 * k)) + 3 * k ^ 2) `div` (6 * k - 2 * p) in (n + k) ^ 3 == n ^ 3 + n ^ 2 * p]

main :: IO ()
main = do
  let goodPrimes = filter isGoodP (takeWhile (< 10 ^ 6) primes)
  for_
    goodPrimes
    ( \p -> do
        when (isGoodP p) $ putStrLn $ "p = " ++ show p ++ "; k = " ++ show (findK p) ++ "; p / k = " ++ show (fromInteger p / fromInteger (findK p))
    )
  print $ length goodPrimes
