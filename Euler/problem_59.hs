{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Control.Monad
import Data.Bits (xor)
import Data.Char (chr, ord)
import Data.List (nub, sort)
import Data.Maybe
import System.IO

repeatList :: [a] -> [a]
repeatList lst = concatMap (const lst) [0 ..]

decrypt :: String -> String -> String
decrypt key cipher =
  [ chr (ord (cipher !! i) `xor` ord (repkey !! i))
    | i <- [0 .. length cipher - 1]
  ]
  where
    repkey = repeatList key

-- Split function
splitBy :: Char -> String -> [String]
splitBy _ "" = []
splitBy delimiterChar inputString = foldr f [""] inputString
  where
    f :: Char -> [String] -> [String]
    f currentChar allStrings@(partialString : handledStrings)
      | currentChar == delimiterChar = "" : allStrings -- start a new partial string at the head of the list of all strings
      | otherwise = (currentChar : partialString) : handledStrings -- add the current char to the partial string

-- Read file contents
fileGetContents :: String -> IO String
fileGetContents path = do
  handle <- openFile path ReadMode
  hGetContents handle

lowercaseLetters :: [Char]
lowercaseLetters = map chr [97 .. 122]

isReadable :: Char -> Bool
isReadable c = c >= chr 32 && c <= chr 125

main :: IO ()
main = do
  -- Read data file
  contents <- fileGetContents "data/data_59.txt"
  -- Parse each line to hands
  let cipher = map (chr . read) (splitBy ',' contents)
  -- Try to maximize the number of e's
  let (es, key, text) =
        maximum
          [ (es, key, text)
            | a <- lowercaseLetters,
              b <- lowercaseLetters,
              c <- lowercaseLetters,
              let key = [a, b, c],
              let text = decrypt key cipher,
              all isReadable text,
              let es = length (filter (== 'e') text)
          ]
  -- Print key and decrypted text
  print (key, text)
  print $ sum (map ord text)
