import Data.List (group, sort)

isPalindrome :: Integer -> Bool
isPalindrome n = show n == reverse (show n)

sumConSquares :: Integer -> Integer -> Integer
sumConSquares n l = sum (map (^ 2) [n .. n + l])

m :: Integer
m = 10 ^ 8

main :: IO ()
main =
  print $
    sum . map head . group $
      sort
        [ s
          | n <- takeWhile (\n -> n ^ 2 < m) [1 ..],
            s <- takeWhile (< m) (map (sumConSquares n) [1 ..]),
            isPalindrome s
        ]
