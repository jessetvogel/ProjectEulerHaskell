count :: Int -> Integer
count = (map f [0 ..] !!)
  where
    f 0 = 1
    f 1 = 1
    f 2 = 1
    f n = 1 + sum [if m < 0 then 1 else count m | skip <- [0 .. n], len <- [3 .. n - skip], let m = n - skip - len - 1] -- either skip evertying, or try a block after skipping `skip` blocks, and of length `len`

main :: IO ()
main = print $ count 50