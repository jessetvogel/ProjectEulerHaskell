import Data.List (sort)
import Euler.Util (fileGetContents, splitBy)

sublists :: Int -> [a] -> [[a]]
sublists 0 _ = [[]]
sublists _ [] = []
sublists n (x : xs) = sublists n xs ++ map (x :) (sublists (n - 1) xs)

isSortedAndUnique :: Ord a => [a] -> Bool
isSortedAndUnique [] = True
isSortedAndUnique [x] = True
isSortedAndUnique (x : y : lst) = x < y && isSortedAndUnique (y : lst)

isSpecialSumSet :: [Int] -> Bool
isSpecialSumSet s = isSortedAndUnique $ concat [sort . map sum $ sublists k s | k <- [1 .. length s]]

main :: IO ()
main = do
  -- Read file
  sets <- map (map (read :: String -> Int) . splitBy ',') . lines <$> fileGetContents "data/data_105.txt"
  -- Only take the special sum sets
  let specialSumSets = filter isSpecialSumSet sets
  -- Print the sum of sums
  print $ sum (map sum specialSumSets)
