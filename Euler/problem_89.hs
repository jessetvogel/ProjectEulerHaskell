import Control.Monad (when)
import Data.Foldable (for_)
import Data.List (group)
import Euler.Util (fileGetContents)

fromRomanChar :: Char -> Integer
fromRomanChar 'I' = 1
fromRomanChar 'V' = 5
fromRomanChar 'X' = 10
fromRomanChar 'L' = 50
fromRomanChar 'C' = 100
fromRomanChar 'D' = 500
fromRomanChar 'M' = 1000
fromRomanChar c = 0

fromRoman :: String -> Integer
fromRoman str =
  let -- Group str by same chars
      parts = group str
      -- Find value of each part
      values = map (sum . map fromRomanChar) parts
      -- Simplify the list of values to something that can be added
      simplify [] = []
      simplify [x] = [x]
      simplify (x : y : lst) = if x > y then x : simplify (y : lst) else (y - x) : simplify lst
      simplified = simplify values
   in sum simplified

toRoman :: Integer -> String
toRoman n
  | n > 1000 = replicate (fromInteger n `div` 1000) 'M' ++ toRoman (n `mod` 1000)
  | h == 9 = "CM" ++ toRoman (n `mod` 100)
  | h == 8 = "DCCC" ++ toRoman (n `mod` 100)
  | h == 7 = "DCC" ++ toRoman (n `mod` 100)
  | h == 6 = "DC" ++ toRoman (n `mod` 100)
  | h == 5 = "D" ++ toRoman (n `mod` 100)
  | h == 4 = "CD" ++ toRoman (n `mod` 100)
  | h == 3 = "CCC" ++ toRoman (n `mod` 100)
  | h == 2 = "CC" ++ toRoman (n `mod` 100)
  | h == 1 = "C" ++ toRoman (n `mod` 100)
  | t == 9 = "XC" ++ toRoman (n `mod` 10)
  | t == 8 = "LXXX" ++ toRoman (n `mod` 10)
  | t == 7 = "LXX" ++ toRoman (n `mod` 10)
  | t == 6 = "LX" ++ toRoman (n `mod` 10)
  | t == 5 = "L" ++ toRoman (n `mod` 10)
  | t == 4 = "XL" ++ toRoman (n `mod` 10)
  | t == 3 = "XXX" ++ toRoman (n `mod` 10)
  | t == 2 = "XX" ++ toRoman (n `mod` 10)
  | t == 1 = "X" ++ toRoman (n `mod` 10)
  | n == 9 = "IX"
  | n == 8 = "VIII"
  | n == 7 = "VII"
  | n == 6 = "VI"
  | n == 5 = "V"
  | n == 4 = "IV"
  | n == 3 = "III"
  | n == 2 = "II"
  | n == 1 = "I"
  | n == 0 = ""
  | otherwise = "(" ++ show n ++ ")"
  where
    h = n `div` 100
    t = n `div` 10

main :: IO ()
main = do
  -- Read file
  romans <- lines <$> fileGetContents "data/data_89.txt"
  -- Compute saved chars by taking differences
  let savedChars = sum $ [length roman - length (toRoman (fromRoman roman)) | roman <- romans]
  -- Print number of saved chars
  print savedChars
