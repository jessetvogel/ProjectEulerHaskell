expansion :: (Integer, Integer) -> [(Integer, Integer)]
expansion (m, n) = (m, n) : expansion (10 * (m `mod` n), n)

cycleLength :: Eq a => [a] -> Integer
cycleLength list = 1 + indexOf (head list) (tail list)

indexOf :: Eq a => a -> [a] -> Integer
indexOf x list = if head list == x then 0 else 1 + indexOf x (tail list)

skip :: Integer -> [a] -> [a]
skip 0 list = list
skip n list = skip (n - 1) (tail list)

main :: IO ()
main = print $ snd $ maximum [(cycleLength (skip 1000 (expansion (1, d))), d) | d <- [1 .. 1000]]
