canMake :: Integer -> [Integer] -> Bool
canMake 0 [] = True
canMake n xs = n >= 0 && n <= makeInteger xs && or [canMake (n - m) (drop k xs) | k <- [1 .. length xs], let m = makeInteger (take k xs)]

makeInteger :: [Integer] -> Integer
makeInteger = foldl (\x y -> 10 * x + y) 0

isSplitting :: Integer -> Bool
isSplitting n = canMake n (map (\c -> read [c]) (show (n ^ 2)))

main :: IO ()
main = print $ sum $ map (^ 2) $ filter isSplitting [2 .. 10 ^ 6]
