import Euler.Util (primeFactorization, primes)

-- Note that p divides R(k) iff 10^k mod p = 1 iff ord(10 mod p) divides k.
-- Now we have k = 10^n = 2^n * 5^n, so p is never a factor of R(10^n) iff
-- ord(10 mod p) has prime factors other than 2 or 5

ord :: Integer -> Integer -> Integer
ord a p = helper a
  where
    helper 1 = 1
    helper a' = 1 + helper ((a * a') `mod` p)

main :: IO ()
main = do
  let ps = takeWhile (< 100000) $ drop 3 primes -- Skip primes 2, 3, 5
  let ps' = filter (not . all (\(p, k) -> p == 2 || p == 5) . primeFactorization . ord 10) ps
  print $ sum ps' + 2 + 3 + 5 -- Don't forget these exceptions!
  -- print ps'
