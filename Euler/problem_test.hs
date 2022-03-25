import Control.Monad (when)
import Data.Foldable (for_)
import Euler.Util (primes)

ord :: Integer -> Integer -> Int
ord a p = helper (a `mod` p)
  where
    helper 1 = 1
    helper b = 1 + helper (b * a `mod` p)

main :: IO ()
main = do
  for_ (takeWhile (<= 68521) primes) (\p -> when (p /= 2 && p /= 5) $ print (p, ord 10 p))