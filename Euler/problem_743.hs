import Euler.Util (modPow)

p :: Integer
p = 1000000007

powersMod :: Integer -> Integer -> [Integer]
powersMod a p = lst where lst = 1 : [(a * b) `mod` p | b <- lst]

a :: Integer -> Integer -> Integer
a n k =
  let t = modPow 4 (n `div` k)
      ps = powersMod t p
      fs = factorialsMod (k `div` 2) p
      kfac = last fs
   in sum [tpow *]

factorialsMod :: Integer -> Integer -> [Integer]
factorialsMod m p = lst where lst = 1 : [(n * f) `mod` p | (n, f) <- zip [1 .. m] lst]

main :: IO ()
main = print 1

-- Let M be a matrix in A(n, k).

-- What do we know?
-- A(n, 0) = 1 (all zeros)
-- A(n, 1) = 2^n
-- A(n, 2) = 2^n + 2
-- A(n, 3) = ??

-- A(n, n) = (2n nCr n)

-- If n is divisible by k, i.e. n = k * m, and k is even, then:
-- Every block of k repeats itself, except that columns [0, 1] and [1, 0] are equivalent.
-- Let j be the number of [0, 1]'s or [1, 0]'s in the first k columns. Then:

-- A(n, k) = sum_{j = 0}^{k} (2^(100 * j)) * (k nCr j) * (k - j nCr (k - j) // 2)
--
--         = sum_{j = 0}^{k // 2} (4^(100 * j)) * k! / (2*j)! / (k // 2 - j)! ^ 2
