import Data.List (sort)

arePermutations :: Integer -> Integer -> Bool
arePermutations a b = sort (show a) == sort (show b)

isqrt :: Integer -> Integer
isqrt = floor . sqrt . fromIntegral

isPrime :: Integer -> Bool
isPrime n = (n > 1) && null [x | x <- [2 .. isqrt n], n `mod` x == 0]

main :: IO ()
main =
  print $
    concatMap
      show
      ( [ [n, n + i, n + i + i]
          | n <- [1000 .. 9999],
            isPrime n,
            i <- [1 .. (9999 - n) `div` 2],
            let a = n + i,
            let b = n + i + i,
            arePermutations n a,
            arePermutations n b,
            isPrime a,
            isPrime b
        ]
          !! 1
      )