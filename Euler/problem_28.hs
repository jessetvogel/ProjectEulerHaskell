m = 1001

spiralSquare :: Integer -> Integer -> Integer -> Integer
spiralSquare n x y
  | (x, y) == (n `div` 2, n `div` 2) = 1
  | x + y <= n && x - y >= 1 = 1 + spiralSquare n (x - 1) y
  | x + y >= n + 1 && x - y >= 0 = 1 + spiralSquare n x (y - 1)
  | x + y >= n - 1 && x - y <= -1 = 1 + spiralSquare n (x + 1) y
  | x + y <= n - 1 && x - y <= 0 = 1 + spiralSquare n x (y + 1)
  | otherwise = 0

-- Note: this is very slow. Use memoization!
main :: IO ()
main = print $ sum ([spiralSquare m x x | x <- [0 .. m - 1]] ++ [spiralSquare m (m - 1 - y) y | y <- [0 .. m - 1]]) - 1
