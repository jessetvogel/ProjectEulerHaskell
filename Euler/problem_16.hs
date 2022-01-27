-- Print the number
main :: IO ()
main = print $ sum (map (\c -> read [c]) (show (2^1000)))
