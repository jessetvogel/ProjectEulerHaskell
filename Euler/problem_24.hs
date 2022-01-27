import Data.List

permutations10 :: [String]
permutations10 = sort (map (map (head . show)) (permutations [0 .. 9]))

main :: IO ()
main = print $ permutations10 !! (1000000 - 1)
