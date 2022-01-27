choose :: Integer -> Integer -> Integer
choose n 0 = 1
choose 0 k = 0
choose n k = choose (n - 1) (k - 1) * n `div` k

main :: IO ()
main = print $ length [(n, r) | n <- [1 .. 100], r <- [0 .. n], choose n r > 1000000]
