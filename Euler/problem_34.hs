factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

isCurious :: Integer -> Bool
isCurious n = sum (map (\c -> factorial (read [c])) (show n)) == n

main :: IO ()
main = print $ sum [n | n <- [3 .. 1000000], isCurious n]
