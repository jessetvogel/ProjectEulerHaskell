import Control.Monad (when)
import Control.Monad.ST (ST, runST)
import Data.Foldable (for_)
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import Data.Vector (Vector, fromList, (!))
import Euler.Util (primeFactorization, primes)

solutions :: Integer -> [(Integer, Integer)]
solutions n = [(x, y) | x <- [n + 1 .. 2 * n], n * x `mod` (x - n) == 0, let y = n * x `div` (x - n)]

seqOfSize :: Int -> Int -> [[Int]]
seqOfSize m 0 = [[]]
seqOfSize m 1 = [[i] | i <- [0 .. m - 1]]
seqOfSize m j = concatMap (\s -> [i : s | i <- [head s + 1 .. m - 1]]) (seqOfSize m (j - 1))

numSolutions :: Integer -> Integer
numSolutions n = do
  let powers = map snd (primeFactorization n) -- compute prime factorization
  numSolutionsFromPrimePowers powers

numSolutionsFromPrimePowers :: [Integer] -> Integer
numSolutionsFromPrimePowers powers = do
  let vec = fromList powers
  let m = length vec
  1
    + sum
      [ prod
        | j <- [1 .. m],
          is <- seqOfSize m j,
          let prod = 2 ^ (j - 1) * product (map (vec !) is)
      ]

nonIncSeqs :: Int -> Integer -> [[Integer]]
nonIncSeqs 0 _ = [[]]
nonIncSeqs 1 i = [[j] | j <- [0 .. i]]
nonIncSeqs m i = concatMap (\j -> map (j :) (nonIncSeqs (m - 1) j)) [0 .. i]

findAnswer :: Integer -> ST s Integer
findAnswer bound = do
  min <- newSTRef (-1)
  for_
    (nonIncSeqs 12 5)
    ( \seq -> do
        let n = product [p ^ k | (p, k) <- zip primes seq]
        min' <- readSTRef min
        when (n < min' || min' == -1) $ do
          let s = numSolutionsFromPrimePowers seq
          when (s > bound) $ writeSTRef min n
    )
  readSTRef min

main :: IO ()
main = do
  --   print $ numSolutionsFromPrimePowers [3, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1]
  let answer = runST $ findAnswer 4000000
  print answer
