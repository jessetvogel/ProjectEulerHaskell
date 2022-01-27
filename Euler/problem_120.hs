r :: Integer -> Integer -> Integer
r a n = ((a - 1) ^ n + (a + 1) ^ n) `mod` (a ^ 2)

rmax :: Integer -> Integer
rmax a = maximum [r a n | n <- [1, 3 .. 2 * a + 1]]

main :: IO ()
main = print $ sum $ map rmax [3..1000]
