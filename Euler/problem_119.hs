import Data.List (sort)

sumDigits :: Integer -> Integer
sumDigits 0 = 0
sumDigits n = n `mod` 10 + sumDigits (n `div` 10)

interestings :: [Integer]
interestings = concatMap (\n -> [nk | k <- [2 .. 20], let nk = n ^ k, sumDigits (n ^ k) == n]) [2 ..]

main :: IO ()
main = print $ sort (take 50 interestings) !! 29
