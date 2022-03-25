import Data.List (nub, sort)
import Euler.Util (divisors, isSquare, isqrt)

-- Starting from r, try values of (u / v) such that d = u / v * r and q = (u / v)^2 * r and n = d * q + r is a square
-- Note that we need d > r, so u > v
-- But also 10^12 >= n = d * q + r = r * ((u/v)^3 * r + 1) implies that (((10^12) / r - 1) / r)^(1/3) * v >= u
fromR :: Integer -> [Integer]
fromR r =
  nub
    [ n
      | v <- divisors r,
        r `mod` (v ^ 2) == 0, -- v must divide r twice
        let umax = ceiling $ ((10 ^ 12 / fromInteger r - 1) / fromInteger r) ** (1 / 3) * fromInteger v,
        u <- [v + 1 .. umax], -- bounds on u
        let d = r * u `div` v,
        let q = d * u `div` v,
        let n = d * q + r,
        isSquare n
    ]

main :: IO ()
main = print $ sum $ nub $ sort $ concatMap fromR [1 .. 10 ^ 6]
