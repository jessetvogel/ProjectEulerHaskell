import Data.List (permutations)

main :: IO ()
main =
  print $
    sum
      [ foldl
          (\b a -> 10 * b + a)
          0
          [d1, d2, d3, d4, d5, d6, d7, d8, d9, d10]
        | [d1, d2, d3, d4, d5, d6, d7, d8, d9, d10] <-
            permutations
              [0, 1, 2, 3, 4, 5, 6, 7, 8, 9],
          even d4
            && (d6 == 0 || d6 == 5)
            && (d3 + d4 + d5) `mod` 3 == 0
            && (d5 * 100 + d6 * 10 + d7) `mod` 7 == 0
            && (d6 - d7 + d8) `mod` 11 == 0
            && (d7 * 100 + d8 * 10 + d9) `mod` 13 == 0
            && (d8 * 100 + d9 * 10 + d10) `mod` 17 == 0
      ]
