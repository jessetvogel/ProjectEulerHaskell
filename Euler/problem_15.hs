choose :: Integer -> Integer -> Integer
choose n 0 = 1
choose 0 k = 0
choose n k = choose (n-1) (k-1) * n `div` k 

-- Recursive function for number of paths
amountPaths :: Integer -> Integer -> Integer
amountPaths x y = choose (x + y) x

-- Print the number of paths on 20 x 20 grid
main :: IO ()
main = print $ amountPaths 20 20
