count :: [Int] -> Int -> Integer
count lengths = helper
  where
    helper = (map f [0 ..] !!)
    f 0 = 1
    f n = 1 + sum [if n' <= 0 then 1 else helper n' | block <- lengths, skip <- [0 .. n - block], let n' = n - skip - block] -- either skip everything or try one of the blocks

main :: IO ()
main = print $ count [2, 3, 4] 50
