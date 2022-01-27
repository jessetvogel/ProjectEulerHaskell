import Euler.Util (primeFactorization)
import Data.List (sort)

rad :: Integer -> Integer
rad n = product $ map fst (primeFactorization n)

main :: IO ()
main = print $ snd $ sort [(r, n) | n <- [1 .. 100000], let r = rad n] !! (10000 - 1)
