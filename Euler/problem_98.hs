import Data.Foldable (for_)
import Data.List (group, sort)
import qualified Data.Map as Data
import Data.Maybe (fromJust, isJust)
import Euler.Util (fileGetContents, splitBy, square)

squaresOfLength :: Int -> [Integer]
squaresOfLength n = map (^ 2) [ceiling (sqrt (10 ^ (n - 1))) .. ceiling (sqrt (10 ^ n)) - 1]

isAnagram :: String -> String -> Bool
isAnagram s t = sort s == sort t && s /= t

anagramsOf :: String -> [String] -> [String]
anagramsOf s = filter (isAnagram s)

findSquarePair :: String -> String -> Maybe Integer
findSquarePair w w' = do
  let n = length w
  let squares = squaresOfLength n
  let sqs =
        concat
          [ [sq, sq']
            | sq <- squares,
              length (group . sort $ w) == length (group . sort . show $ sq), -- Number of distinct digits should be equal to number of distinct letters
              let m = match w (show sq),
              map (m Data.!) w == show sq, -- Make sure mapping works!
              let sq' = read (map (m Data.!) w'),
              sq' `elem` squares
          ]
  if null sqs
    then Nothing
    else Just (maximum sqs)

match :: String -> String -> Data.Map Char Char
match a b = Data.fromList (zip a b)

main :: IO ()
main = do
  -- Load words
  words <- map (init . tail) . splitBy ',' <$> fileGetContents "data/data_98.txt"
  -- Print the desired number
  print $ maximum [fromJust sq | word <- words, a <- anagramsOf word words, let sq = findSquarePair word a, isJust sq]
