import Data.Bifunctor (bimap)
import Data.List (elem)

overlap :: Eq a => [a] -> [a] -> [a]
overlap as bs = [x | x <- as, x `elem` bs]

isRedundant :: [Int] -> [Int] -> Bool
isRedundant as bs = not (null (overlap as bs)) || all (uncurry (<)) (zip as bs) || all (uncurry (>)) (zip as bs)

pairs :: Int -> Int -> [([Int], [Int])]
pairs n k = map (bimap reverse reverse) (helper n k)
  where
    helper n 1 = [([i], [j]) | i <- [1 .. n], j <- [i + 1 .. n]]
    helper n k = concatMap (\(as, bs) -> [(i : as, j : bs) | i <- [head as + 1 .. n], i `notElem` bs, j <- [head bs + 1 .. n], i /= j, j `notElem` as]) (helper n (k - 1))

main :: IO ()
main = print $ length $ filter (not . uncurry isRedundant) $ concatMap (pairs 12) [1 .. 12]
