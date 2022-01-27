m :: Int
m = 50

count :: Int -> Integer
count = (map f [0 ..] !!)
  where
    f 0 = 1
    f 1 = 1
    f 2 = 1
    f n = 1 + sum [if n' < 0 then 1 else count n' | skip <- [0 .. n], len <- [m .. n - skip], let n' = n - skip - len - 1] -- either skip evertying, or try a block after skipping `skip` blocks, and of length `len`

main :: IO ()
main = print $ head [n | n <- [1 ..], count n > 1000000]
