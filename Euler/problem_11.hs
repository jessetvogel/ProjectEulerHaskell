import Control.Monad
import System.IO

-- Read file contents
fileGetContents :: String -> IO String
fileGetContents path = do
  handle <- openFile path ReadMode
  hGetContents handle

-- Quadruples
tuples :: Int -> Int -> Int -> [[(Int, Int)]]
tuples k w h = [[(x + i, y) | i <- [0 .. k - 1]] | x <- [0 .. w - k - 1], y <- [0 .. h - 1]] ++ [[(x, y + i) | i <- [0 .. k - 1]] | x <- [0 .. w - 1], y <- [0 .. h - k - 1]] ++ [[(x + i, y + i) | i <- [0 .. k - 1]] | x <- [0 .. w - k - 1], y <- [0 .. h - k - 1]] ++ [[(x + k - 1 - i, y + i) | i <- [0 .. k - 1]] | x <- [0 .. w - k - 1], y <- [0 .. h - k - 1]]

-- Maximum product
maximumProduct :: [[Integer]] -> Integer
maximumProduct m = maximum [product [m !! fst x !! snd x | x <- q] | q <- tuples 4 (length (head m)) (length m)]

-- Load file and determine maximum number
main :: IO ()
main = do
  contents <- fileGetContents "data/data_11.txt"
  let numbers = map (map (read :: String -> Integer) . words) (lines contents)
  print (maximumProduct numbers)
