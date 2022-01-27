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

-- Name score
nameScore :: String -> Int
nameScore name = sum [if c == '"' then 0 else ord c - ord 'A' + 1 | c <- name]

-- Be clever about searching for shortest path
main :: IO ()
main = do
  -- Read data file
  contents <- fileGetContents "data/data_22.txt"
  -- Split to get names
  let names = sort (splitBy ',' contents)
  -- Print desired number
  print $ sum [nameScore (names !! (i - 1)) * i | i <- [1 .. length names]]
