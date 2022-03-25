import Euler.Util (isSquare)

-- Golden nugget for n with 5n^2 + 2n + 1 a perfect square
-- Turns out to be F_(2n) * F_(2n + 1)

fib :: [Integer]
fib = 0 : 1 : [a + b | (a, b) <- zip fib (tail fib)]

main :: IO ()
main = print $ [fib !! (2 * n) * fib !! (2 * n + 1) | n <- [0 ..]] !! 15
