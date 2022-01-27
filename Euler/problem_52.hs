import Data.List (sort)

arePermutations :: Integer -> Integer -> Bool
arePermutations a b = sort (show a) == sort (show b)

main :: IO ()
main =
  print $
    head
      [ n
        | n <- [1 ..],
          arePermutations n (2 * n),
          arePermutations n (3 * n),
          arePermutations n (4 * n),
          arePermutations n (5 * n),
          arePermutations n (6 * n)
      ]
