import Data.List (sort)
import Data.Ratio (numerator)

relevantFractions :: Integer -> [Rational]
relevantFractions m = [fromInteger n / fromInteger d | d <- [1 .. m], let n = d * 3 `div` 7, n * 7 /= 3 * d]

main :: IO ()
main = print $ numerator $ maximum (relevantFractions 1000000)