import Data.Vector (Vector, fromList, (!))
import Euler.Util (fileGetContents, splitBy)

type Matrix = Vector (Vector Integer)

matrix :: IO Matrix
matrix = do
  -- Read data file
  contents <- fileGetContents "data/data_83.txt"
  -- Parse matrix
  let m = map (map (read :: String -> Integer) . splitBy ',') (lines contents)
  -- Convert lsit of lists to an array
  return $ fromList (map fromList m)

around :: Int -> Int -> Int -> Int -> [(Int, Int)]
around w h x y = filter (\(x', y') -> x' >= 0 && y' >= 0 && x' < w && y' < h) [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

nextMatrix :: Matrix -> Matrix -> Matrix
nextMatrix orig prev =
  fromList
    [ fromList
        [ if (x, y) == (0, 0)
            then orig ! 0 ! 0
            else orig ! x ! y + minimum (map (\(x', y') -> prev ! x' ! y') (around w h x y))
          | y <- [0 .. h - 1]
        ]
      | x <- [0 .. w - 1]
    ]
  where
    w = length prev
    h = length (prev ! 0)

iterate' :: Int -> (a -> a) -> a -> a
iterate' 0 _ x = x
iterate' n f x = iterate' (n - 1) f (f x)

main :: IO ()
main = do
  -- Read matrix
  m <- matrix
  -- Initial path length matrix
  let plMatrix = fromList [fromList [if (x, y) == (0, 0) then m ! 0 ! 0 else 999999999999 | y <- [0 .. 79]] | x <- [0 .. 79]]
  -- Compute matrix with minimal path lengths
  let minPathMatrix = iterate' 200 (nextMatrix m) plMatrix
  -- Print the number
  print $ minPathMatrix ! 79 ! 79
