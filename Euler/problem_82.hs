import Data.Vector (Vector, fromList, (!))
import Euler.Util (fileGetContents, splitBy)

type Matrix = Vector (Vector Integer)

matrix :: IO Matrix
matrix = do
  -- Read data file
  contents <- fileGetContents "data/data_82.txt"
  -- Parse matrix
  let m = map (map (read :: String -> Integer) . splitBy ',') (lines contents)
  -- Convert lsit of lists to an array
  return $ fromList (map fromList m)

transpose :: Matrix -> Matrix
transpose m =
  fromList
    [fromList [m ! y ! x | y <- [0 .. w -1]] | x <- [0 .. h - 1]]
  where
    w = length m; h = length (m ! 0)

nextColumn :: Vector Integer -> Vector Integer -> Vector Integer
nextColumn prev new =
  fromList
    [ minimum
        ( [ prev ! y' + sum (map (new !) (if y' < y then [y' .. y] else [y .. y']))
            | y' <- [0 .. n - 1]
          ]
        )
      | y <- [0 .. n - 1]
    ]
  where
    n = length new

main :: IO ()
main = do
  -- Read matrix (transposed)
  m <- transpose <$> matrix
  -- Size of matrix
  let n = length m
  -- Compute min path column-wise
  let minSumColumns = fromList $ (m ! 0) : [nextColumn (minSumColumns ! (x - 1)) (m ! x) | x <- [1 .. n - 1]]
  -- Take the minima of the last column
  print $ minimum (minSumColumns ! (n - 1))
