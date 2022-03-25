import Data.Foldable (for_)
import Euler.Util (divisors, isSquare, isqrt, primeFactorization)

sumGaussianDivisors :: Integer -> Integer
sumGaussianDivisors n = do
    
  | p `mod` 4 == 1 =
    head
      [ 2 * a + 2 * b
        | a <- [1 .. isqrt p],
          let b = isqrt (p - a ^ 2),
          b ^ 2 == p - a ^ 2
      ]
  | otherwise = p

sumDivisors :: Integer -> Integer
sumDivisors n = do
  let pks = primeFactorization n
  product $ map (\(p, k) -> (p ^ (k + 1) - 1) `div` (p - 1)) pks

main :: IO ()
-- main = do
--   for_
--     [1 .. 1000]
--     ( \n -> do
--         let s = sum $ divisors n
--         let s' = sumDivisors n

--         print $ s - s'
--     )
main = print $ sumDivisorsGaussian 5
