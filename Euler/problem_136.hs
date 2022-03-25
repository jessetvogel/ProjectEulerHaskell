import Control.Monad (when)
import Data.Foldable (for_)
import Data.List (group, sort)
import Euler.Util (divisors, isqrt, primeFactorization, primes)
import Util (isSingleton)

findXYZ :: Integer -> [(Integer, Integer, Integer)]
findXYZ n =
  [ (x, y, z)
    | let maxp = fromInteger $ isqrt (3 * n),
      p <- filter (<= maxp) $ divisors n,
      let q = n `div` p,
      (p + q) `mod` 4 == 0,
      let k = (p + q) `div` 4,
      let x = q + k,
      let y = x - k,
      let z = x - 2 * k,
      x > 0,
      y > 0,
      z > 0
  ]

main :: IO ()
-- main = print $ length [findXYZ n | n <- [1 .. 50 * 10 ^ 6], isSingleton (findXYZ n)]
main = do
  let solutions = [(n, xyzs) | n <- [1 .. 50 * 10 ^ 6], let xyzs = findXYZ n, isSingleton xyzs]
  for_
    solutions
    ( \(n, xyzs) -> do
        putStrLn $ "n = " ++ show n ++ ", xyz = " ++ show xyzs
    )
  putStrLn $ "# = " ++ show (length solutions)
