import Data.Foldable (for_)
import Euler.Util (isPrime)

-- Builds numbers of length n with digit d repeated m times
builder :: Int -> Int -> Int -> [Integer]
builder d 1 0 = [toInteger d' | d' <- [1 .. 9], d' /= d]
builder d 1 1 = [toInteger d | d /= 0]
builder d n m
  | m < 0 || n <= 0 || m > n = [] -- weird exceptions
  | otherwise = [10 * k + toInteger d | k <- builder d (n - 1) (m - 1)] ++ [10 * k + toInteger d' | k <- builder d (n - 1) m, d' <- [0 .. 9], d /= d'] -- Last digit is either d or not d

findPrimes :: Int -> Int -> Int -> [Integer]
findPrimes n d m = [p | p <- builder d n m, isPrime p]

findM :: Int -> Int -> Int
findM n d = head [m | m <- [n - 1, n - 2 .. 0], not (null (findPrimes n d m))]

findN :: Int -> Int -> Int -> Int
findN n d m = length (findPrimes n d m)

findS :: Int -> Int -> Int -> Integer
findS n d m = sum (findPrimes n d m)

main :: IO ()
main = do
  let n = 10
  print $ sum [findS n d m | d <- [0 .. 9], let m = findM n d]

-- for_
--   [0 .. 9]
--   ( \d -> do
--       let m = findM n d
--       putStrLn $ "M(" ++ show n ++ ", " ++ show d ++ ") = " ++ show m
--       putStrLn $ "N(" ++ show n ++ ", " ++ show d ++ ") = " ++ show (findN n d m)
--       putStrLn $ "S(" ++ show n ++ ", " ++ show d ++ ") = " ++ show (findS n d m)
--   )
