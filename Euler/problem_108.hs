import Control.Monad (when)
import Data.Foldable (for_)
import Euler.Util (eulerPhi, primeFactorization)

solutions :: Integer -> [(Integer, Integer)]
solutions n = [(x, y) | x <- [n + 1 .. 2 * n], n * x `mod` (x - n) == 0, let y = n * x `div` (x - n)]

-- numDivisors :: Integer -> Integer
-- numDivisors n = product $ map (\(p, k) -> k + 1) (primeFactorization n)

main :: IO ()
main = do
  print $
    minimum
      [ n
        | f <- [0 .. 1],
          e <- [0 .. 1],
          d <- [0 .. 2],
          c <- [0 .. 4],
          b <- [0 .. 5],
          a <- [0 .. 5],
          let n = 2 ^ a * 3 ^ b * 5 ^ c * 7 ^ d * 11 ^ e * 13 ^ f, -- experimentally, we want lots of factors in n
          n < 277200, -- this was not the right answer, so should be lower than thist
          let m = length $ solutions n,
          m > 1000
      ]
