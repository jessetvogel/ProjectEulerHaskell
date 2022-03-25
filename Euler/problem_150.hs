import Data.Foldable (for_)
import Data.Vector (Vector, fromList, (!))
import Euler.Util (isqrt, triangle)

-- Test case
-- rows = 6
-- s = fromList [15, -14, -7, 20, -13, -5, -3, 8, 23, -26, 1, -4, -5, -18, 5, -16, 31, 2, 9, 28, 3]

rows :: Int
rows = 1000

s :: Vector Integer
s = fromList $ 273519 : [((s ! (k - 1) + 2 ^ 19) * 615949 + 797807) `mod` 2 ^ 20 - 2 ^ 19 | k <- [1 .. rows * (rows + 1) `div` 2]]

-- List of rows below a given position k
rowsBelow :: Int -> [[Int]]
rowsBelow k =
  [ [ k'
      | x <- [0 .. row - row_k],
        let k' = k + (row - row_k) * ((row - row_k) + 1) `div` 2 + (row - row_k) * (row_k - 1) + x
    ]
    | row <- [row_k .. rows]
  ]
  where
    row_k = fromInteger (isqrt (8 * toInteger k - 7) + 1) `div` 2 -- row of k

-- Find the `minimum triangle` below a given position k
minTriangleBelow :: Int -> Integer
minTriangleBelow k = minimum triangleSums
  where
    -- triangleSums is the list of sums of triangles below k
    triangleSums = scanl1 (+) (map (sum . map (\k' -> s ! (k' - 1))) (rowsBelow k))

main :: IO ()
main = do
  -- For every possible starting position, compute minimal triangle, then choose the minimum of those
  let range = [1 .. rows * (rows + 1) `div` 2]
  let triangles = map minTriangleBelow range
  for_ (zip range triangles) print
  print $ minimum triangles
