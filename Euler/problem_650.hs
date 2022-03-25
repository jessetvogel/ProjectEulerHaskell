import Control.Monad (unless, when)
import Data.Foldable (for_)
import Euler.Util (modInv, modPow, primeFactorization)
import Hoopl.Dataflow (Fact)

-- pascal :: [[Integer]]
-- pascal = [1] : [1 : [a + b | (a, b) <- zip row (tail row)] ++ [1] | row <- pascal]

-- n `choose` k = n! / k! / (n - k)! = (k + 1) * ... * n / (1 * 2 * ... * (n - k))

-- \Prod_{k = 0}^{n} n `choose` k
-- \Prod_{k = 0}^{n} 1 * 2 * ... * n / (1 * .. * k) / (1 * ... * (n - k))
-- 1^(n + 1) * 2^(n + 1) * ... * n^(n + 1) / ((1) * (1 * 2) * (1 * 2 * 3) * ... * (1 * 2 * ... * n)) / ((1 * ... * n) * ... * (1))

-- The relevant numbers are:
-- f(n) = 1 * 2 * ... * n
-- t(n) = (1) * (1 * 2) * ... * (1 * 2 * ... * n)
-- b(n) = f(n)^(n + 1) / t(n)^2

type Factorization = [(Integer, Integer)]

-- Prime factorizations of the factorials 0!, 1!, 2!, ...
factF :: [Factorization]
factF = [] : [factAdd f (primeFactorization n) | (n, f) <- zip [1 ..] factF]

-- Prime factorizations of the sequence t(n)
factT :: [Factorization]
factT = drop 1 $ scanl factAdd [] factF

-- Prime factorizations of the sequence b(n)
factB :: [Factorization]
factB = [factAdd (factMul (n + 1) f) (factMul (-2) t) | (n, f, t) <- zip3 [0 ..] factF factT]

factAdd :: Factorization -> Factorization -> Factorization
factAdd [] qs = qs
factAdd ps [] = ps
factAdd ps qs
  | p < q = (p, k) : factAdd (tail ps) qs
  | q < p = (q, l) : factAdd ps (tail qs)
  | otherwise = if k + l /= 0 then (p, k + l) : factAdd (tail ps) (tail qs) else factAdd (tail ps) (tail qs)
  where
    (p, k) = head ps
    (q, l) = head qs

factMul :: Integer -> Factorization -> Factorization
factMul _ [] = []
factMul n ((p, k) : ps) = (p, n * k) : factMul n ps

-- addAllFacts :: [[(Integer, Integer)]] -> [(Integer, Integer)]
-- addAllFacts = foldl factAddacts []

-- Computes (p^(k + 1) - 1) / (p - 1) = 1 + ... + p^k

factSumDivisors :: Factorization -> Integer
factSumDivisors ps = product [helper p k | (p, k) <- ps] `mod` 1000000007
  where
    helper :: Integer -> Integer -> Integer
    helper p k = ((modPow p (k + 1) 1000000007 - 1) * modInv (p - 1) 1000000007) `mod` 1000000007

main :: IO ()
main = do
  let m = 20000
  let bs = take m $ drop 1 $ map factSumDivisors factB
  for_ (zip [1 ..] bs) print
  print $ sum bs `mod` 1000000007
