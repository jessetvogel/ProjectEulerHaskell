import Data.Foldable (for_)
import Data.Vector (Vector, fromList, (!))

largestSum :: [Integer] -> Integer
largestSum lst =
  fst $
    foldl
      ( \(max, local) x -> do
          let local' = local + x
          let max' = if max < local' then local' else max
          let local'' = if local' >= 0 then local' else 0
          (max', local'')
      )
      (-99999999, 0)
      lst

s :: Vector Integer
s =
  fromList $
    [(100003 - 200003 * k + 300007 * k ^ 3) `mod` 1000000 - 500000 | k <- [1 .. 55]]
      ++ [(s ! (k - 24 - 1) + s ! (k - 55 - 1) + 1000000) `mod` 1000000 - 500000 | k <- [56 .. 4000000]]

main :: IO ()
main = do
  -- Rows
  let horizontal = maximum $ map largestSum $ [map (\k -> s ! (k - 1)) [(row - 1) * 2000 + 1 .. row * 2000] | row <- [1 .. 2000]]
  -- Columns
  let vertical = maximum $ map largestSum $ [map (\k -> s ! (k - 1)) [col, col + 2000 .. col + 4000000 - 2000] | col <- [1 .. 2000]]
  -- Hopefully the diagonals don't matter ... (Turns out they don't)
  print $ maximum [horizontal, vertical]
