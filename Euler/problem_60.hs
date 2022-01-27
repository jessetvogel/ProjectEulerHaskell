primes :: [Integer]
primes = sieve [2 ..] where sieve l = if null l then [] else p : sieve [x | x <- tail l, x `mod` p /= 0] where p = head l

primesBelow :: Integer -> [Integer]
primesBelow n = helper primes where helper l = if h >= n then [] else h : helper (tail l) where h = head l

isPrime :: Integer -> Bool
isPrime n = (n > 1) && null [x | x <- [2 .. isqrt n], n `mod` x == 0]

isqrt :: Integer -> Integer
isqrt = floor . sqrt . fromIntegral

main :: IO ()
main =
  print $
    sum $
      head
        [ [p, q, r, s, t]
          | p <- primes,
            q <- primesBelow p,
            isPrime (read (show p ++ show q)),
            isPrime (read (show q ++ show p)),
            r <- primesBelow q,
            isPrime (read (show p ++ show r)),
            isPrime (read (show r ++ show p)),
            isPrime (read (show q ++ show r)),
            isPrime (read (show r ++ show q)),
            s <- primesBelow r,
            isPrime (read (show p ++ show s)),
            isPrime (read (show s ++ show p)),
            isPrime (read (show q ++ show s)),
            isPrime (read (show s ++ show q)),
            isPrime (read (show r ++ show s)),
            isPrime (read (show s ++ show r)),
            t <- primesBelow s,
            isPrime (read (show p ++ show t)),
            isPrime (read (show t ++ show p)),
            isPrime (read (show q ++ show t)),
            isPrime (read (show t ++ show q)),
            isPrime (read (show r ++ show t)),
            isPrime (read (show t ++ show r)),
            isPrime (read (show s ++ show t)),
            isPrime (read (show t ++ show s))
        ]