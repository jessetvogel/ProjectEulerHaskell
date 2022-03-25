import Euler.Pell (quadraticSquares)
import Euler.Util (isqrt)

main :: IO ()
main = do
  let t = head $ filter (> 10 ^ 12) $ quadraticSquares (2, -2, 1)
  let b = (isqrt (2 * t * (t - 1) + 1) + 1) `div` 2
  print b

-- P(BB) = b * (b - 1) / t * (t - 1) = 1 / 2

-- Gives b * (b - 1) = 1 / 2 * t * (t - 1)

-- Solve for b = 1/2 (sqrt(2 * t * (t - 1) + 1) + 1), so 2 * t * (t - 1) + 1 should be a square
