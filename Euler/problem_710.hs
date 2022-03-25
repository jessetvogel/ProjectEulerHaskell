-- (a, b, b, a)
-- (a, b, c, b, a)

-- # { n = a + b + ... + b + a with at least one 2 }
t :: Integer -> Integer
t n = sum [(p'' !! fromInteger k) * p (n - 2 * k - 4) | k <- [0 .. n `div` 2]]

-- # { n = a + b + ... + b + a without any 2 }
p' :: Integer -> Integer
p' n =
  if even n
    then sum [partitions' ((n - k) `div` 2) | k <- [0, 2 .. n]]
    else sum [partitions' ((n - k) `div` 2) | k <- [1, 3 .. n]]

p'' :: [Integer]
p'' = 0 : 1 : 2 : [2 * a - b + c | (a, b, c) <- zip3 (tail (tail p'')) (tail p'') p'']

-- # { n = a + b + ... + b + a }
p :: Integer -> Integer
p n = 2 ^ (n `div` 2)

-- p n =
--   if even n
--     then sum [partitions ((n - k) `div` 2) | k <- [0, 2 .. n]]
--     else sum [partitions ((n - k) `div` 2) | k <- [1, 3 .. n]]

-- # { n = a + b + c + ... without any 2 }
partitions' :: Integer -> Integer
partitions' 0 = 1
partitions' n = sum [partitions' (n - k) | k <- [1 .. n], k /= 2]

-- # { n = a + b + c + ... }
partitions :: Integer -> Integer
partitions 0 = 1
partitions n = 2 ^ (n - 1)

-- partitions n = sum [partitions (n - k) | k <- [1 .. n]]

main :: IO ()
-- main = print $ map p' [1 .. 20]
main = print $ take 10 p''
