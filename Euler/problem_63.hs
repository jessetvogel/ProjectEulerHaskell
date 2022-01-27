import Euler.Util

ofLength :: Show a => Int -> [a] -> [a]
ofLength l lst = filter (\x -> length (show x) == l) $ listUntil (\x -> length (show x) > l) lst

nthPowers :: Int -> [Integer]
nthPowers n = [k ^ n | k <- [1 ..]]

main :: IO ()
main = print $ length $ concatMap (\n -> ofLength n (nthPowers n)) [1 .. 100]