-- Better count the non-bouncy numbers, that is, the increasing and decreasing numbers below 10^100

choose :: Integer -> Integer -> Integer
choose n 0 = 1
choose 0 k = 0
choose n k = choose (n -1) (k -1) * n `div` k

-- An increasing number of length n can be seen as choosing 9 'bars' out of (n + 9) 'bars and dots'
-- 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
-- A decreasing number of length n can be seen as choosing 10 'bars' out of (n + 10) 'bars and dots'
-- 0 | 9 | 8 | 7 | 6 | 5 | 4 | 3 | 2 | 1 | 0
-- A constant number has a length [1 .. n] and a digit [1 .. 9]
amountNonBouncyOfLength :: Integer -> Integer
amountNonBouncyOfLength n =
  (choose (n + 9) 9 - 1) -- increasing numbers: the number 0 is not included
    + (choose (n + 10) 10 - n - 1) -- decreasing numbers: for some reason should subtract n + 1
    - (9 * n) -- constant numbers

main :: IO ()
main = do
  print $ amountNonBouncyOfLength 100
