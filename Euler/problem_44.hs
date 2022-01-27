isqrt :: Integer -> Integer
isqrt = floor . sqrt . fromIntegral

pentagonal :: Integer -> Integer
pentagonal n = n * (3 * n - 1) `div` 2

isPentagonal :: Integer -> Bool
isPentagonal p = p > 0 && p == pentagonal ((1 + isqrt (24 * p + 1)) `div` 6)

m = 100000

main :: IO ()
main = print $ head [pentagonal k - pentagonal j | j <- [1 .. m], k <- [j .. m], isPentagonal (pentagonal j + pentagonal k) && isPentagonal (pentagonal k - pentagonal j)]
