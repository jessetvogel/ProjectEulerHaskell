isCurious :: Integer -> Integer -> Integer -> Bool
isCurious a b c = (10 * a + b) * c == (10 * b + c) * a

prod :: [(Integer, Integer)] -> (Integer, Integer)
prod [] = (1, 1)
prod l = (a * c, b * d) where (a, b) = head l; (c, d) = prod (tail l)

main :: IO ()
main = print $ d `div` n where (n, d) = prod [(a, c) | a <- [1 .. 9], b <- [1 .. 9], c <- [1 .. 9], isCurious a b c && a /= b]
