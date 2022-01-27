import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Data.Ratio (denominator, numerator)
import Euler.Util (isSquare)

contFracSqrt :: Integer -> [Integer]
contFracSqrt n = helper n (1, 0)
  where
    helper n (a, b) = c : helper n (a / d, (fromInteger c - b) / d)
      where
        c = floor (fromRational a * sqrt (fromInteger n) + fromRational b)
        d = a ^ 2 * fromInteger n - (b - fromInteger c) ^ 2

contFracSqrtExt :: Integer -> [(Rational, Rational)]
contFracSqrtExt n = helper n (1, 0)
  where
    helper n (a, b) = (a, b) : helper n (a / d, (fromInteger c - b) / d)
      where
        c = floor (fromRational a * sqrt (fromInteger n) + fromRational b)
        d = a ^ 2 * fromInteger n - (b - fromInteger c) ^ 2

period :: Eq a => [a] -> Int
period lst = 1 + fromJust (elemIndex (head lst) (tail lst))

contFracToRational :: [Integer] -> Rational
contFracToRational lst = foldl (\e a -> fromInteger a + 1 / e) (fromInteger $ head rlst) (tail rlst) where rlst = reverse lst

minimalPell :: Integer -> Integer
minimalPell d = do
  let per = period (drop 10 $ contFracSqrtExt d)
  let qs = [contFracToRational (take (per * i) $ contFracSqrt d) | i <- [1 ..]]
  let xys = [(x, y) | q <- qs, let x = numerator q, let y = denominator q, x ^ 2 - d * y ^ 2 == 1]
  let x = fst $ head xys
  x

-- For loop construction
for :: [a] -> (a -> IO ()) -> IO ()
for [] _ = return ()
for (x : xs) f = do f x; for xs f

main :: IO ()
main =
  do
    -- Compute the minimal solutions for x
    let minimalXs = [(x, d) | d <- [2 .. 1000], not $ isSquare d, let x = minimalPell d]
    -- Compute the value of D for which x is maximal
    let d = snd $ maximum minimalXs
    -- Print value of d
    print d