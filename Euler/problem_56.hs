sumOfDigits :: Integer -> Integer
sumOfDigits n = sum $ map (\c -> read [c]) (show n)

main :: IO ()
main = print $ maximum [sumOfDigits (a ^ b) | a <- [1 .. 99], b <- [1 .. 99]]
