import Data.List (group, sort)
import Euler.Util (listUntil, primes)

m :: Integer
m = 50000000

primeSquares :: [Integer]
primeSquares = map (^ 2) primes

primeCubes :: [Integer]
primeCubes = map (^ 3) primes

primeQuads :: [Integer]
primeQuads = map (^ 4) primes

main :: IO ()
main =
  print $
    length $
      group $
        sort $
          [ a + b + c
            | a <- listUntil (> m) primeQuads,
              b <- listUntil (> m - a) primeCubes,
              c <- listUntil (> m - a - b) primeSquares
          ]
