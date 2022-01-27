import Data.List (sort)

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
main =
  putStrLn $
    concatMap show . head . filter isSpecialSumSet $
      [ [a, b, c, d, e, f, g]
        | a <- [17 .. 21], -- 19
          b <- [28 .. 32], -- 30
          c <- [35 .. 39], -- 37
          d <- [36 .. 40], -- 38
          e <- [37 .. 41], -- 39
          f <- [39 .. 43], -- 41
          g <- [42 .. 46] -- 44
      ]
