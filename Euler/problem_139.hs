pythagoreans :: [(Integer, Integer, Integer)]
pythagoreans =
  [ (a, b, c)
    | m <- [1 ..],
      n <- [1 .. m - 1],
      gcd m n == 1,
      not (odd m && odd n),
      let a = m ^ 2 - n ^ 2,
      let b = 2 * m * n,
      let c = m ^ 2 + n ^ 2
  ]

main :: IO ()
main = do
  let primitives = takeWhile (\(a, b, c) -> a + b + c < 10 ^ 8) $ filter (\(a, b, c) -> c `mod` abs (b - a) == 0) pythagoreans
  let many = [10 ^ 8 `div` (a + b + c) | (a, b, c) <- primitives]
  print $ sum many
