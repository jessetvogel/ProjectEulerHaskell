import Data.List (nub)

relevantFractions :: Integer -> [Rational]
relevantFractions m =
  [ q
    | d <- [2 .. m],
      let min = d `div` 3,
      let max = d `div` 2,
      n <- [min .. max],
      n * 3 > d && n * 2 < d,
      gcd n d == 1,
      let q = fromInteger n / fromInteger d
  ]

main :: IO ()
main = print $ length $ relevantFractions 12000
