numReversibles :: Integer -> Integer
numReversibles len
  | even len = 20 * (30 ^ ((len `div` 2) - 1))
  | len `mod` 4 == 3 = 100 * (500 ^ (len `div` 4))
  | otherwise = 0

main :: IO ()
main = print $ sum $ map numReversibles [1..9]
