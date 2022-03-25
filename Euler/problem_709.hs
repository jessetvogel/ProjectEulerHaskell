import Data.Foldable (for_)
import Euler.Util (choose)
import Control.Monad (when)

-- A packing can be encoded by a sequence:

-- 1

-- 1, 2

-- 1, 2, 3
-- 3, 1, 2

-- 1, 2, 3, 4
-- 3, 1, 2, 4
-- 2, 4, 1, 3
-- 3, 4, 1, 2
-- 1, 4, 2, 3

-- 1, 2, 3, 4, 5
-- 3, 1, 2, 4, 5
-- 2, 4, 1, 3, 5
-- 3, 4, 1, 2, 5
-- 1, 4, 2, 3, 5
-- 1, 2, 5, 3, 4
-- 1, 3, 5, 2, 4
-- 1, 4, 5, 2, 3
-- 5, 1, 2, 3, 4
-- 5, 3, 1, 2, 4
-- 5, 2, 4, 1, 3
-- 5, 3, 4, 1, 2
-- 5, 1, 4, 2, 3

-- So upper bound is n!

-- Let g(n, k) = # packings of n bags resulting in k final bags (1 \le k \le n)
-- E.g. g(n, n) = 1
-- Then f(n) = \sum_{k = 1}^{n} g(n, k)
-- Also, g(n, k) = \sum_{j = 1}^{k} g(n - 1, j) * (j `choose` (k - 1)) provided (j - k + 1) is even

-- Have 17 bags, want 4, need to pick (17 - 4 + 1) bags to put into the new bag,

p :: Integer
p = 1020202009

f :: Integer -> Integer
f n = 0

g :: [[Integer]]
g = [1] : [[sum [a * (j `choose` (k - 1)) | (j, a) <- zip [1 ..] g', odd (j + k)] `mod` p | k <- [1 .. n]] | (n, g') <- zip [2 ..] g]

main :: IO ()
main = do
  for_ (zip [1 .. 20] g) (\(n, a) -> print (n, reverse a))

-- print $ take 10 g
