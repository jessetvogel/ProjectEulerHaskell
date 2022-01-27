import Data.List (group, nub, sort)

type Dart = (Char, Int)

newtype Throw = Throw {darts :: [Dart]}

scoreDart :: Dart -> Int
scoreDart (c, v)
  | c == 'S' = v
  | c == 'D' = 2 * v
  | c == 'T' = 3 * v
  | otherwise = 0

scoreThrow :: Throw -> Int
scoreThrow = sum . map scoreDart . darts

main :: IO ()
main = do
  -- Construct all checkouts
  let checkouts =
        [Throw {darts = [('D', v)]} | v <- [1 .. 20] ++ [25]]
          ++ [ Throw {darts = [("SDT" !! i1, v1), ('D', v2)]}
               | v1 <- [1 .. 20] ++ [25],
                 v2 <- [1 .. 20] ++ [25],
                 i1 <- [0 .. 2],
                 v1 /= 25 || i1 /= 2 -- (triple bulls-eye does not exist)
             ]
          ++ [ Throw {darts = [("SDT" !! i1, v1), ("SDT" !! i2, v2), ('D', v3)]}
               | v1 <- [1 .. 20] ++ [25],
                 v2 <- [1 .. 20] ++ [25],
                 i1 <- [0 .. 2],
                 i2 <- [i1 .. 2],
                 (v1 /= 25 || i1 /= 2) && (v2 /= 25 || i2 /= 2), -- (triple bulls-eye does not exist)
                 i1 < i2 || v1 <= v2, -- first two darts can be interchanged to get the equivalent throws
                 v3 <- [1 .. 20] ++ [25]
             ]

  print $ length $ filter (\t -> scoreThrow t < 100) checkouts
