isqrt :: Integer -> Integer
isqrt = floor . sqrt . fromIntegral

triangle :: Integer -> Integer
triangle n = n * (n + 1) `div` 2

pentagonal :: Integer -> Integer
pentagonal n = n * (3 * n - 1) `div` 2

hexagonal :: Integer -> Integer
hexagonal n = n * (2 * n - 1)

isTriangle :: Integer -> Bool
isTriangle t = t > 0 && t == triangle ((isqrt (8 * t + 1) - 1) `div` 2)

isPentagonal :: Integer -> Bool
isPentagonal p = p > 0 && p == pentagonal ((1 + isqrt (24 * p + 1)) `div` 6)

isHexagonal :: Integer -> Bool
isHexagonal h = h > 0 && h == hexagonal ((isqrt (8 * h + 1) + 1) `div` 4)

main :: IO ()
main = print $ filter (\n -> isTriangle n && isPentagonal n) (map hexagonal [1 ..]) !! 2
