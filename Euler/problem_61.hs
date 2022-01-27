import Data.List (permutations)

isqrt :: Integer -> Integer
isqrt = floor . sqrt . fromIntegral

triangle :: Integer -> Integer
triangle n = n * (n + 1) `div` 2

isTriangle :: Integer -> Bool
isTriangle t = t > 0 && t == triangle ((isqrt (8 * t + 1) - 1) `div` 2)

square :: Integer -> Integer
square n = n * n

isSquare :: Integer -> Bool
isSquare n = n == square (isqrt n)

pentagonal :: Integer -> Integer
pentagonal n = n * (3 * n - 1) `div` 2

isPentagonal :: Integer -> Bool
isPentagonal p = p > 0 && p == pentagonal ((1 + isqrt (24 * p + 1)) `div` 6)

hexagonal :: Integer -> Integer
hexagonal n = n * (2 * n - 1)

isHexagonal :: Integer -> Bool
isHexagonal h = h > 0 && h == hexagonal ((isqrt (8 * h + 1) + 1) `div` 4)

heptagonal :: Integer -> Integer
heptagonal n = n * (5 * n - 3) `div` 2

isHeptagonal :: Integer -> Bool
isHeptagonal n = n == heptagonal ((3 + isqrt (40 * n + 9)) `div` 10)

octagonal :: Integer -> Integer
octagonal n = n * (3 * n - 2)

isOctagonal :: Integer -> Bool
isOctagonal n = n == octagonal ((1 + isqrt (3 * n + 1)) `div` 3)

listUntil :: Integer -> [Integer] -> [Integer]
listUntil n l = if h > n then [] else h : listUntil n (tail l) where h = head l

triangles :: [Integer]
triangles = filter (>= 1000) $ listUntil 9999 (map triangle [0 ..])

squares :: [Integer]
squares = filter (>= 1000) $ listUntil 9999 (map square [0 ..])

pentagonals :: [Integer]
pentagonals = filter (>= 1000) $ listUntil 9999 (map pentagonal [0 ..])

hexagonals :: [Integer]
hexagonals = filter (>= 1000) $ listUntil 9999 (map hexagonal [0 ..])

heptagonals :: [Integer]
heptagonals = filter (>= 1000) $ listUntil 9999 (map heptagonal [0 ..])

octagonals :: [Integer]
octagonals = filter (>= 1000) $ listUntil 9999 (map octagonal [0 ..])

main :: IO ()
main =
  print $
    sum $
      head
        [ [x, y, z, w, a, b]
          | perm <- permutations [triangles, squares, pentagonals, hexagonals, heptagonals, octagonals],
            x <- head perm,
            y <- perm !! 1,
            x `mod` 100 == y `div` 100,
            z <- perm !! 2,
            y `mod` 100 == z `div` 100,
            w <- perm !! 3,
            z `mod` 100 == w `div` 100,
            a <- perm !! 4,
            w `mod` 100 == a `div` 100,
            b <- perm !! 5,
            a `mod` 100 == b `div` 100,
            b `mod` 100 == x `div` 100
        ]
