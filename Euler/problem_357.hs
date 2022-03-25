-- Which numbers can we skip?
-- All numbers which are divisible by a square can be skipped, since, if n = m * k * k, then k + n / k = (1 + m) * k, will not be prime!
-- Also, 1 + n must be prime, so we can just check for n = p - 1 for a prime p

-- If n is even, then 2 + n / 2 must be prime.

import Data.Foldable (for_)
import Data.IntSet (fromList, member)
import Euler.MillerRabin (isPrime)
import Euler.Util (divisors)
import System.Random (RandomGen, newStdGen)

primes :: RandomGen g => g -> [Integer]
primes g = filter (isPrime g) (2 : [3, 5 ..])

ns :: RandomGen g => g -> [Integer]
ns g =
  let lst = map (\p -> p - 1) $ takeWhile (<= 10 ^ 8 + 1) (primes g)
   in filter (\n -> all (isPrime g) [d + n `div` d | d <- divisors n, d > 1, 2 * d <= n]) lst

main :: IO ()
main = do
  seed <- newStdGen
  let ns' = ns seed
  for_ ns' print
  print $ sum ns'
