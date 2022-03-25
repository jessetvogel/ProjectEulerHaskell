-- https://crypto.stanford.edu/pbc/notes/contfrac/pell.html

module Euler.Pell where

import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Data.Ratio (denominator, numerator)
import Euler.Util (isqrt)

data ContFrac = ContFrac {a :: [Integer], remainders :: [(Rational, Rational)]}

-- Constructs the continued fraction of \sqrt{n}
contFracSqrt :: Integer -> ContFrac
contFracSqrt n =
  let (a, remainders) = unzip $ helper 1 0
   in ContFrac {a = a, remainders = remainders}
  where
    -- Current remainder is encoded as a * \sqrt{n} + b, where a, b \in \QQ
    helper a b = (c, (a, b)) : helper (a / d) ((fromInteger c - b) / d)
      where
        c = floor (fromRational a * sqrt (fromInteger n) + fromRational b)
        d = a ^ 2 * fromInteger n - (b - fromIntegral c) ^ 2

-- Takes the k'th term of a continued fraction and computes it as a rational
contFracToRational :: Int -> ContFrac -> Rational
contFracToRational k cf =
  foldl (\e a -> fromInteger a + 1 / e) (fromInteger $ head ras) (tail ras)
  where
    ras = reverse (take k (a cf))

-- Find the period of a continued fraction
period :: ContFrac -> Int
period cf = 1 + fromJust (elemIndex (head lst) (tail lst))
  where
    lst = drop 10 (remainders cf)

-- Find all solutions (x, y) to the Pell equation x^2 - d * y^2 = 1
pell :: Integer -> [(Integer, Integer)]
pell d =
  let cf = contFracSqrt d
      k = period cf
      appr = contFracToRational k cf
      (p, q) = (numerator appr, denominator appr)
      solutions = (p, q) : [(p * p' + q * q' * d, p * q' + p' * q) | (p', q') <- solutions]
   in filter (\(x, y) -> x ^ 2 - d * y ^ 2 `elem` [-1, 1]) solutions

-- Find all solutions (x, y) to the general Pell equation x^2 - d * y^2 = n
generalPell :: Integer -> Integer -> [(Integer, Integer)]
generalPell d n =
  let pell_d = pell d
      (u, v) = head pell_d -- First Pell solution
      (x, y) = head [(x, y) | y <- [1 ..], let x2 = n + d * y ^ 2, let x = isqrt x2, x ^ 2 == x2] -- Smallest solution
      (x', y') = (u * x + v * y * d, u * y + v * x) -- Next solution after multiplying with Pell solution
      -- (l1, l2) = if n > 0 then (0, floor $ sqrt (fromInteger (n * (u - 1)) / fromInteger (2 * d))) else (ceiling $ sqrt (- fromInteger n / fromInteger d), floor $ sqrt (fromInteger (- n * (u + 1)) / fromInteger (2 * d)))
      -- xys = [(x, y) | y <- [l1 .. l2], let x2 = n + d * y ^ 2, let x = isqrt x2, x ^ 2 == x2] -- Brute-force some non-trivial solutions
      xys = (x, y) : [(x'', y'') | y'' <- [y + 1 .. y' - 1], let x''2 = n + d * y'' ^ 2, let x'' = isqrt x''2, x'' ^ 2 == x''2] -- Brute-force all solutions (x'', y'') with y < y'' < y'
   in xys
        ++ concatMap
          (\(p, q) -> map (\(x, y) -> (p * x + q * y * d, p * y + q * x)) xys)
          pell_d -- Generate all others from the usual Pell equation

-- Finds all values of x such that ax^2 + bx + c is a square
quadraticSquares :: (Integer, Integer, Integer) -> [Integer]
quadraticSquares (a, b, c) =
  let d = 4 * a -- discriminant
      n = 4 * a * (d * c - b ^ 2) -- rhs number
      solutions =
        [ x
          | (u, v) <- generalPell d n,
            let y = u `div` d,
            let x = (v - b) `div` (2 * a),
            a * x ^ 2 + b * x + c == y ^ 2
        ]
   in solutions
