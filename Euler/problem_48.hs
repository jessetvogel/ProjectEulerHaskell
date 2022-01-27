main :: IO ()
main = print $ sum [n ^ n | n <- [1 .. 1000]] `mod` 10^10