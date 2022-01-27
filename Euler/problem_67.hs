import Control.Monad
import Data.Char
import System.IO

-- Read file contents
fileGetContents :: String -> IO String
fileGetContents path = do
  handle <- openFile path ReadMode
  hGetContents handle

-- Be clever about searching for shortest path
main :: IO ()
main = do
  -- Read data file
  contents <- fileGetContents "data/data_67.txt"
  let numbers = map (map (read :: String -> Integer) . words) (lines contents)
  let h = length numbers

  -- Compute longest path from bottom up
  let longest =
        last numbers :
          [ [ (numbers !! (h - 1 - row) !! x) + max (longest !! (row - 1) !! x) (longest !! (row - 1) !! (x + 1))
              | x <- [0 .. h - 1 - row]
            ]
            | row <- [1 .. h - 1]
          ]

  -- Print the length of the longest route starting from the beginning
  print $ last $ last longest
