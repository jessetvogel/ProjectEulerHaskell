import Data.List (sort)
import Euler.Pell (contFracSqrt, contFracToRational, generalPell, pell, period, quadraticSquares)
import Euler.Util (isSquare, isqrt)

-- From L = sqrt(h^2 + (b/2)^2) and h = b \pm 1 follows that
-- b must be even, so b = 2a, and hence
-- L = sqrt(5a^2 \pm 4a + 1), so 5a^2 \pm 4a + 1 must be a square

main :: IO ()
main = do
  let l1s = filter (> 1) [isqrt (5 * a ^ 2 + 4 * a + 1) | a <- quadraticSquares (5, 4, 1)]
  let l2s = filter (> 1) [isqrt (5 * a ^ 2 - 4 * a + 1) | a <- quadraticSquares (5, -4, 1)]

  print $ sum $ take 12 $ sort (take 12 l1s ++ take 12 l2s)
