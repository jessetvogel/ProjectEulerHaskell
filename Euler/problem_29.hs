import Data.Set as Set

main :: IO ()
main = print $ size (Set.fromList [a ^ b | a <- [2 .. 100], b <- [2 .. 100]])
