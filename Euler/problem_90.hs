import Data.List (nub, permutations)

options :: Int -> Int -> [Int]
options 6 6 = [66, 69, 96, 99]
options 6 9 = [66, 69, 96, 99]
options 9 6 = [66, 69, 96, 99]
options 9 9 = [66, 69, 96, 99]
options a 6 = [10 * a + 6, 60 + a, 10 * a + 9, 90 + a]
options a 9 = [10 * a + 6, 60 + a, 10 * a + 9, 90 + a]
options 6 b = [10 * b + 6, 60 + b, 10 * b + 9, 90 + b]
options 9 b = [10 * b + 6, 60 + b, 10 * b + 9, 90 + b]
options a b = [10 * a + b, 10 * b + a]

orderedZip :: [a] -> [(a, a)]
orderedZip [] = []
orderedZip (x : xs) = [(x, y) | y <- xs] ++ orderedZip xs

main :: IO ()
main =
  do
    let dice =
          [ [a, b, c, d, e, f]
            | a <- [0 .. 9],
              b <- [a + 1 .. 9],
              c <- [b + 1 .. 9],
              d <- [c + 1 .. 9],
              e <- [d + 1 .. 9],
              f <- [e + 1 .. 9]
          ]

    let solutions =
          [ 1
            | (d, d') <- orderedZip dice,
              let pairs = concatMap (uncurry options) [(a, b) | a <- d, b <- d'],
              all (`elem` pairs) [1, 4, 9, 16, 25, 36, 49, 64, 81]
          ]

    print $ length solutions
