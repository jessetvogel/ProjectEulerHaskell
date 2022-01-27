import Data.Vector (Vector, fromList, toList, (!))

sumDigitsSquared :: Int -> Int
sumDigitsSquared n = sum $ map ((^ 2) . (\c -> read [c])) (show n)

finish :: Int -> Vector Int
finish n = vec
  where
    vec =
      fromList
        [ f
          | k <- [0 .. n],
            let f
                  | k == 0 = 0
                  | k == 1 = 1
                  | k == 89 = 89
                  | otherwise = vec ! sumDigitsSquared k
        ]

main :: IO ()
main = do
  -- Generate results for numbers up to 600
  let vec500 = finish 600
  -- Print
  print $ length $ filter (== 89) $ map (\k -> vec500 ! sumDigitsSquared k) [1 .. 10000000]
