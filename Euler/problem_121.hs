import Euler.Util (factorial)

-- 2 * ... * 15 * 16
-- all blue = 1
-- 14 blue =

sublists :: Int -> [a] -> [[a]]
sublists 0 _ = [[]]
sublists _ [] = []
sublists n (x : xs) = sublists n xs ++ map (x :) (sublists (n - 1) xs)

chance :: Int -> Rational
chance n = do
  let denom = factorial (toInteger (n + 1))
  let numer = sum (map (helper n) [0 .. (n - 1) `div` 2])
  toRational numer / toRational denom
  where
    helper n r = sum $ map (product . map (\k -> k - 1)) (sublists r [2 .. toInteger n + 1])

main :: IO ()
main = print $ floor (1 / chance 15)

-- bbbb = 1/2 * 1/3 * 1/4 * 1/5 = 1/120
-- bbbr = 1/2 * 1/3 * 1/4 * 4/5 = 4/120
-- bbrb = 1/2 * 1/3 * 3/4 * 1/5 = 3/120
-- brbb = 1/2 * 2/3 * 1/4 * 1/5 = 2/120
-- rbbb = 1/2 * 1/3 * 1/4 * 1/5 = 1/120

-- + = 11/120
