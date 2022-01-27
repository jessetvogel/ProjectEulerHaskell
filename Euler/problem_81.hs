import Data.Array (Array, array, listArray, (!))
import Data.Array.Base (IArray (numElements))
import Euler.Util (fileGetContents, splitBy)

type Matrix = Array Int (Array Int Integer)

fromList :: [a] -> Array Int a
fromList lst = listArray (0, length lst - 1) lst

matrix :: IO Matrix
matrix = do
  -- Read data file
  contents <- fileGetContents "data/data_81.txt"
  -- Parse matrix
  let m = map (map (read :: String -> Integer) . splitBy ',') (lines contents)
  -- Convert lsit of lists to an array
  return $ fromList (map fromList m)

minPathSum :: Matrix -> Matrix
minPathSum m = mat
  where
    (w, h) = matrixSize m
    mat =
      array (0, w) $
        [ ( x,
            array (0, h) $
              [ (y, v)
                | y <- [0 .. h],
                  let v
                        | x == 0 && y == 0 = m ! x ! y
                        | x == 0 = m ! x ! y + mat ! x ! (y - 1)
                        | y == 0 = m ! x ! y + mat ! (x - 1) ! y
                        | otherwise = m ! x ! y + min (mat ! (x - 1) ! y) (mat ! x ! (y - 1))
              ]
          )
          | x <- [0 .. w]
        ]

matrixSize :: Matrix -> (Int, Int)
matrixSize m = (length m, length (m ! 0))

main :: IO ()
main = do
  m <- matrix
  let (w, h) = matrixSize m
  let mp = minPathSum m
  print $ mp ! (w - 1) ! (h - 1)
  return ()
