import Control.Exception (evaluate)
import Data.Ratio (numerator)

type Polynomial = [Rational]

multiply :: Polynomial -> Polynomial -> Polynomial
multiply p q = [sum [(p !! i) * (q !! (k - i)) | i <- [max 0 (k - m) .. min n k]] | k <- [0 .. n + m]]
  where
    n = length p - 1 -- degree of p
    m = length q - 1 -- degree of q

add :: Polynomial -> Polynomial -> Polynomial
add p q =
  if n > m
    then [a + b | (a, b) <- zip p (q ++ [0, 0 ..])]
    else [a + b | (a, b) <- zip (p ++ [0, 0 ..]) q]
  where
    n = length p
    m = length q

lagrange :: [Integer] -> Polynomial
lagrange u = foldl add [0] terms
  where
    n = length u - 1
    terms = [map (ui *) (lpoly n i) | i <- [0 .. n], let ui = toRational (u !! i)]
    lpoly n i = foldl multiply [1] factors
      where
        factors = [[- j' / (i' - j'), 1 / (i' - j')] | j <- [0 .. n], j /= i, let i' = toRational i + 1, let j' = toRational j + 1]

eval :: Polynomial -> Integer -> Rational
eval p x = sum [a * toRational x ^ k | (a, k) <- zip p [0 ..]]

main :: IO ()
main = do
  let u = [1 - n + n ^ 2 - n ^ 3 + n ^ 4 - n ^ 5 + n ^ 6 - n ^ 7 + n ^ 8 - n ^ 9 + n ^ 10 | n <- [1 .. 20]]

  let bops = [lagrange (take k u) | k <- [1 .. 10]]
  let fits = [eval p (k + 1) | (k, p) <- zip [1 ..] bops]

  print $ numerator $ sum fits
