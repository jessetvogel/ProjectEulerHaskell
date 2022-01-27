import Data.List (permutations, subsequences)
import Euler.Util (isPrime)

constructPrimes :: [Int] -> [(Integer, [Int])]
constructPrimes digits =
  [ (p, remaining)
    | subseq <- subsequences ds, -- we want the number we construct to at least contains the first digit (it must be used some time), and some subset of the remaining digits
      perm <- permutations (d : subseq), -- take some permutation of these digits
      let p = read (concatMap show perm), -- create a number out of it
      isPrime p, -- check as early as possible whether or not it is a prime
      let remaining = [d' | d' <- digits, d' /= d && d' `notElem` subseq] -- list the remaining digits
  ]
  where
    d = head digits; ds = tail digits

primeSets :: [Int] -> [[Integer]]
primeSets digits =
  concatMap
    ( \(p, remaining) -> do
        if null remaining
          then [[p]] -- if there are no digits remaining, the set is just [p]
          else map (p :) (primeSets remaining) -- if there are digits remaining, compute prime sets with those digits, and prepend p
    )
    (constructPrimes digits)

main :: IO ()
main = print $ length (primeSets [1, 2, 3, 4, 5, 6, 7, 8, 9])
