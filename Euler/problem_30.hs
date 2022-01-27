isSumOfFifthPowersOfDigits :: Integer -> Bool
isSumOfFifthPowersOfDigits n = n == sum (map (\c -> read [c] ^ 5) (show n))

main :: IO ()
main = print $ sum [n | n <- [2 .. 400000], isSumOfFifthPowersOfDigits n]
