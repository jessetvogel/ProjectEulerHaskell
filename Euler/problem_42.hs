{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Control.Monad
import Data.Char
import Data.List
import System.IO

-- Read file contents
fileGetContents :: String -> IO String
fileGetContents path = do
  handle <- openFile path ReadMode
  hGetContents handle

-- Split function
splitBy :: Char -> String -> [String]
splitBy _ "" = []
splitBy delimiterChar inputString = foldr f [""] inputString
  where
    f :: Char -> [String] -> [String]
    f currentChar allStrings@(partialString : handledStrings)
      | currentChar == delimiterChar = "" : allStrings -- start a new partial string at the head of the list of all strings
      | otherwise = (currentChar : partialString) : handledStrings -- add the current char to the partial string

-- Triangle things
triangleNumbers :: [Int]
triangleNumbers = [n * (n + 1) `div` 2 | n <- [1 .. 100]]

isTriangleNumber :: Int -> Bool
isTriangleNumber n = n `elem` triangleNumbers

isTriangleWord :: String -> Bool
isTriangleWord word = isTriangleNumber (sum [if c == '"' then 0 else ord c - ord 'A' + 1 | c <- word])

-- Be clever about searching for shortest path
main :: IO ()
main = do
  -- Read data file
  contents <- fileGetContents "data/data_42.txt"
  -- Print number of triangle words
  print $ length (filter isTriangleWord (splitBy ',' contents))
