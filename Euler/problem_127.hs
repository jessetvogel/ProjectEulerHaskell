import Data.Foldable (for_)
import Data.List (intersect)
import Euler.Util (primeFactorization, primes)

maxC :: Integer
maxC = 120000 - 1

-- maxC = 20000 - 1
-- maxC = 1000 - 1

clearFactors :: Integer -> Integer -> Integer
clearFactors n p
  | n `mod` p == 0 = clearFactors (n `div` p) p
  | otherwise = n

primeDivisors :: Integer -> [Integer]
primeDivisors n = helper n primes
  where
    helper 1 _ = []
    helper m ps =
      if m `mod` p == 0
        then p : helper (clearFactors (m `div` p) p) ps
        else helper m (tail ps)
      where
        p = head ps

-- abcHits :: [(Integer, Integer, Integer)]
-- abcHits =
--   [ (a, b, c)
--     | a <- [1 .. maxC `div` 2],
--       let primesA = primeDivisors a,
--       b <- [a + 1 .. maxC - a],
--       not $ any (\p -> b `mod` p == 0) primesA,
--       let primesB = primeDivisors b,
--       let c = a + b,
--       let radAB = product (primesA ++ primesB),
--       radAB < c, -- quick breakpoint
--       let radABC = radAB * product (primeDivisors c),
--       radABC < c
--   ]

productsOfUntil :: [Integer] -> Integer -> [Integer]
productsOfUntil [] n = [1]
productsOfUntil ps n = concatMap (\pk -> map (pk *) $ productsOfUntil (tail ps) (n `div` pk)) ppowers
  where
    p = head ps
    ppowers = takeWhile (<= n) $ map (p ^) [0 ..]

abcHits :: [(Integer, Integer, Integer)]
abcHits =
  [ (a, b, c)
    | c <- [1 .. maxC], -- Loop over c
      let primesC = primeDivisors c, -- Find primes dividing c
      let radC = product primesC, -- Compute rad(c)
      radC < c, -- At least must have rad(c) < c
      let cDivRadC = c `div` radC, -- rad(a) * rad(b) cannot exceed this value
      let possiblePrimesA = filter (\p -> c `mod` p /= 0) $ takeWhile (< cDivRadC) primes, -- a can only contain primes below rad(c) / c, which don't divide c
      a <- productsOfUntil possiblePrimesA (c `div` 2), -- construct numbers with those prime factors below c / 2 (since a < b)
      let b = c - a,
      a < b,
      let radABC = radC * product (primeDivisors b) * product (primeDivisors a),
      radABC < c
  ]

main :: IO ()
main = print $ sum $ map (\(a, b, c) -> c) abcHits

-- main = do
--   for_
--     abcHits
--     ( \(a, b, c) -> do
--         print c
--     )
--   print $ sum $ map (\(a, b, c) -> c) abcHits