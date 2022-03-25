import Data.List (nub)
import Euler.Util (divisors)

findXYZ :: Integer -> [(Integer, Integer, Integer)]
findXYZ n =
  nub
    [ (x, y, z)
      | p <- divisors n,
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
main = print $ length [findXYZ n | n <- [1 .. 10 ^ 6], length (findXYZ n) == 10]
