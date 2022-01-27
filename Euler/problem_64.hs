import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Euler.Util (isSquare)

q :: Rational
q = 1 / 2 - 1 / 3

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

main :: IO ()
main = print $ length $ filter odd [period $ drop 10 $ contFracSqrtExt n | n <- [2 .. 10000], not $ isSquare n]
