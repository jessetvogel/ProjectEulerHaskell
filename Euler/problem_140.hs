import Euler.Pell (generalPell, quadraticSquares)
import Euler.Util (isSquare)

-- A_G(x) = \frac{x \left(G_{1} x - G_{1} - G_{2} x\right)}{x^{2} + x - 1}
--        = \frac{x \left(- 3 x - 1\right)}{x^{2} + x - 1}
--
-- A_G(x) = n solving for x gives x = (n + 1 - sqrt(5*n^2 + 14*n + 1)) / (2*n + 6)
--
-- Hence, a golden nugget for n with 5*n^2 + 14*n + 1 a perfect square

main :: IO ()
main = do
  let ns = filter (> 0) $ quadraticSquares (5, 14, 1)
  print $ sum $ take 30 ns
