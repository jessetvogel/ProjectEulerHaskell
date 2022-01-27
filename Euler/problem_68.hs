import Data.List (permutations)

-- Magic 5 gons with given sum
magicFiveGons :: Integer -> [[Integer]]
magicFiveGons s =
  [ [a, f, g, b, g, h, c, h, i, d, i, j, e, j, f]
    | [a, b, c, d, e, f, g, h, i, j] <- permutations [1 .. 10],
      minimum [a, b, c, d, e] == a,
      a + f + g == s,
      b + g + h == s,
      c + h + i == s,
      d + i + j == s,
      e + j + f == s
  ]

main :: IO ()
main = do
  let solutions =
        concat
          [ map (read :: String -> Integer) digit16Strings
            | n <- [14 .. 25],
              let solutions = magicFiveGons n,
              let digit16Strings = filter (\s -> length s == 16) $ map (concatMap show) solutions
          ]
  print $ maximum solutions
