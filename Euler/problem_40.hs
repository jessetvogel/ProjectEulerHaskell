expansion :: [Integer]
expansion = map (\c -> read [c]) (concatMap show [1 ..])

main :: IO ()
main = print $ product (map (\i -> expansion !! (i - 1)) [1, 10, 100, 1000, 10000, 100000, 1000000])
