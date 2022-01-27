import Euler.Util (primesBelow)

memo :: (Int -> Int -> a) -> [[a]]
memo f = map (\x -> map (f x) [0 ..]) [0 ..]

-- Number of ways to write n with prime numbers <= k
partitions :: [[Integer]]
partitions = memo part
  where
    part 0 k = 1 -- 0 can be uniquely partitioned
    part n k = sum $ map (\p -> partitions !! (n - fromIntegral p) !! fromIntegral p) (primesBelow (fromIntegral (min n k) + 1))

main :: IO ()
main = print $ head $ [n | n <- [1 ..], let m = partitions !! n !! (n - 1), m > 5000]
