import Data.Set as Set

m = 28123

-- m = 24

-- Divisors
divisors :: Integer -> [Integer]
divisors n = [d | d <- [1 .. (n - 1)], n `mod` d == 0]

-- Is abundant
isAbundant :: Integer -> Bool
isAbundant n = sum (divisors n) > n

-- The list of abundant numbers
abundants :: [Integer]
abundants = [n | n <- [12 .. m], isAbundant n]

-- The set of sums of two abundants numbers
sumsOfTwoAbundants :: Set Integer
sumsOfTwoAbundants = Set.fromList [a + b | a <- abundants, b <- abundants, a + b <= m]

-- Print the desired sum
main :: IO ()
main = print $ sum [1 .. m] - sum sumsOfTwoAbundants
