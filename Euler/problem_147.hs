straights :: Integer -> Integer -> Integer
straights w h = sum [(w - x + 1) * (h - y + 1) | x <- [1 .. w], y <- [1 .. h]]

acrosses :: Integer -> Integer -> Integer
acrosses w h = 

main :: IO ()
main = print $ straights 3 2