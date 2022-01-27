import Data.Ratio (denominator, numerator)
import Euler.Util (isSquare, isqrt)

-- Approximates the square root of n using Newton's method
newtonSqrt :: Integer -> Rational -> Rational
newtonSqrt n q = q - (q ^ 2 - fromInteger n) / (2 * q)

approxSqrt :: Integer -> [Rational]
approxSqrt n = fromIntegral (isqrt n) : map (newtonSqrt n) (approxSqrt n)

decimalDigits :: Rational -> String
decimalDigits q = digits n
  where
    n = numerator q
    d = denominator q
    digits r = show (r `div` d) ++ digits (10 * (r `mod` d))

stableElement :: Eq a => [a] -> a
stableElement lst =
  if x == y
    then x
    else stableElement (tail lst)
  where
    x = head lst; y = lst !! 1

digitalSum :: String -> Integer
digitalSum s = sum $ (\c -> read [c]) <$> s

main :: IO ()
main = do
  let nonsquares = filter (not . isSquare) [1 .. 100]
  let digits = map (stableElement . map (take 100 . decimalDigits) . approxSqrt) nonsquares
  let sums = map digitalSum digits
  print (sum sums)
