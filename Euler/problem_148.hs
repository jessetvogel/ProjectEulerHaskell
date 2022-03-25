import Control.Monad (when)
import Data.Foldable (for_)
import qualified Data.Vector.Mutable as M

-- The number of numbers that are not divisible by 7 on the rows of Pascals triangle are given by
-- product [ a * b * c * ... | a <- [1 .. 7], b <- [1 .. 7], c <- [1 .. 7], ... ]

-- Function for computing single cases
row :: Integer -> Integer
row 0 = 1
row n = (n `mod` 7 + 1) * row (n `div` 7)

-- Use recursive symmetry
sumRows :: Integer -> Integer
sumRows n =
  if n < 7
    then sum (map row [0 .. n - 1])
    else 28 * sumRows (n `div` 7) + sum (map row [(n `div` 7) * 7 .. n - 1])

main :: IO ()
main = do
  print $ sumRows (10 ^ 9)
