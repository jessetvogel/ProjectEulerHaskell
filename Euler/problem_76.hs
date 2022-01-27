memo :: (Int -> Int -> a) -> [[a]]
memo f = map (\x -> map (f x) [0 ..]) [0 ..]

-- Number of ways to write n with numbers at most k
partitions :: [[Integer]]
partitions = memo part
  where
    part 0 k = 1 -- 0 can be uniquely partitioned
    part n 1 = 1 -- n can be partitioned in one way using only ones
    part n k = sum $ map (\x -> partitions !! (n - x) !! x) [1 .. min n k]

main :: IO ()
main = print $ partitions !! 100 !! 99
