import Euler.Util (primes)

extGCD :: Integer -> Integer -> (Integer, Integer, Integer)
extGCD 0 0 = error "extGCD(0,0) is undefined"
extGCD a 0 = (1, 0, a) -- Base case
extGCD a b =
  let (q, r) = a `quotRem` b -- q and r of a/b
      (c, x, y) = extGCD b r -- Recursive call
   in (x, c - q * x, y) -- Recursive results

consecutivePrimes :: [(Integer, Integer)]
consecutivePrimes = zip primes (tail primes)

main :: IO ()
main = do
  let cps = takeWhile (\(p1, p2) -> p1 <= 1000000) $ drop 2 consecutivePrimes
  let ss =
        [ s
          | (p1, p2) <- cps,
            let r = length (show p1),
            let (inv10r, _, _) = extGCD (10 ^ r) p2,
            let k = (- p1 * inv10r) `mod` p2,
            let s = p1 + 10 ^ r * k
        ]
  print $ sum ss
