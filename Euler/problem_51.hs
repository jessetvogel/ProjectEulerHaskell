import Data.List (nub)

primes :: [Integer]
primes = sieve [2 ..] where sieve l = if null l then [] else p : sieve [x | x <- tail l, x `mod` p /= 0] where p = head l

isPrime :: Integer -> Bool
isPrime n = (n > 1) && null [x | x <- [2 .. isqrt n], n `mod` x == 0]

isqrt :: Integer -> Integer
isqrt = floor . sqrt . fromIntegral

replace :: Char -> Char -> Char -> Char
replace a b c = if c == a then b else c

primesFromTemplate :: String -> [Integer]
primesFromTemplate s =
  [ p
    | r <- "0123456789",
      let s' = map (replace '*' r) s,
      head s' /= '0',
      let p = read s',
      isPrime p
  ]

hasPrimeCousins :: Int -> Integer -> Bool
hasPrimeCousins m n = any (\l -> length l == m) [primesFromTemplate (map (replace d '*') $ show n) | d <- nub $ show n]

main :: IO ()
main = print $ head [p | p <- primes, hasPrimeCousins 8 p]
