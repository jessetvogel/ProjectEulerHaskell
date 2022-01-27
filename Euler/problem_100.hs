import Euler.Util (isSquare, isqrt)

-- a(n) = 6*a(n-1) - a(n-2) - 2, n >= 2, a(0) = 1, a(1) = 4.
mySequence :: [Integer]
mySequence = 1 : 4 : [6 * y - x - 2 | (x, y) <- zip mySequence (tail mySequence)]

main :: IO ()
main = do
  let t = head $ filter (> 10 ^ 12) mySequence
  let b = (isqrt (2 * t ^ 2 - 2 * t + 1) + 1) `div` 2
  print b

-- P(BB) = b * (b - 1) / t * (t - 1) = 1 / 2

-- Gives b * (b - 1) = 1 / 2 * t * (t - 1)

-- Solve for b = 1/2 (sqrt(2 * t * (t - 1) + 1) + 1), so 2 * t * (t - 1) + 1 should be a square
