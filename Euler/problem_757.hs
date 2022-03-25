-- N = a * b = c * d and a + b = c + d + 1

-- Write a = x/2 + y/2, b = x/2 - y/2, and c = u/2 + v/2, d = u/2 - v/2
-- Then x = u + 1, so solve for u = x - 1
-- Then a = x/2 + y/2, b = x/2 - y/2, and c = (x - 1)/2 + v/2, d = (x - 1)/2 - v/2
--
-- Now, N = a * b = (x^2 - y^2) / 4 = (u^2 - v^2) / 4 = (x^2 - 2x + 1 - v^2) / 4

-- Hence, y^2 = v^2 + 2x - 1, that is, x = (y^2 - v^2 + 1) / 2

-- y < x <= (y^2 + 1) / 2

-- Range over y in [0..]
-- Range over v "= sqrt((y^2 + 1) / 2 - x)"" < sqrt((y - 1)^2 / 2) = (y - 1) / sqrt(2)
-- Let x = (y^2 - v^2 + 1) / 2 (parity check on v)
-- Let u = x - 1

-- What is the minimal value of v ??
-- Well, N = a * b = (x^2 - y^2) / 4 = (((y^2 - v^2 + 1) / 2)^2 - y^2) / 4 = [...], so v < ...

import Data.List (sort)
import qualified Data.Set as Set
import Euler.Util (isqrt)

nubOrd :: Ord a => [a] -> [a]
nubOrd xs = go Set.empty xs
  where
    go s (x : xs)
      | x `Set.member` s = go s xs
      | otherwise = x : go (Set.insert x s) xs
    go _ _ = []

m :: Integer
m = 10 ^ 8

solutions :: [(Integer, Integer, Integer, Integer, Integer)]
solutions =
  [ (n, y, b, c, d)
    | y <- [0 ..],
      let vmax = floor $ (fromInteger y - 1) / 1.414,
      let vmin = isqrt (max 0 (y ^ 2 + 1 - isqrt (16 * m + 4 * y ^ 2))),
      v <- [vmax, vmax - 1 .. max 0 vmin],
      odd (y + v),
      let x = (y ^ 2 - v ^ 2 + 1) `div` 2,
      y < x,
      let u = x - 1,
      v < u,
      let a = (x - y) `div` 2,
      let b = (x + y) `div` 2,
      let c = (u - v) `div` 2,
      let d = (u + v) `div` 2,
      let n = a * b
  ]

main :: IO ()
main = do
  let ns = filter (<= m) $ map (\(n, y, _, _, _) -> y) $ take (10000) solutions
  print $ length $ nubOrd ns
  print $ last ns
--   print ns
