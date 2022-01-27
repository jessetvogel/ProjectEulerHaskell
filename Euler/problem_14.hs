import Data.Foldable
import Data.Ord
-- The length of a '3n + 1 sequence'
collatzLength :: Integer -> Integer
collatzLength 1 = 1
collatzLength n = if even n then 1 + collatzLength (n `div` 2) else 1 + collatzLength (3*n + 1)

-- Print the starting number with the longest chain
main :: IO ()
main = print $ fst $ maximumBy (comparing snd) (map (\n -> (n, collatzLength n)) [1..1000000])
