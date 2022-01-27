count :: Int -> Int -> Integer
count len = helper
  where
    helper = (map f [0 ..] !!)
    f 0 = 1
    f n = 1 + sum [if n' <= 0 then 1 else helper n' | skip <- [0 .. n - len], let n' = n - skip - len] -- either skip evertying, or try a block after skipping `skip` blocks, and of length `len`

m :: Int
m = 50

main :: IO ()
main = print $ (count 2 50 - 1) + (count 3 50 - 1) + (count 4 50 - 1)
