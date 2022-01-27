import Data.List (sort)
import Data.Ratio (denominator, numerator)
import Euler.Util (isSquare, isqrt)

isIntSolution :: Integer -> Integer -> Integer -> Bool
isIntSolution x y z =
  isSquare p && isSquare q && (isqrt p + isqrt q) `mod` d == 0
  where
    [x', z', y'] = sort [x, z, y] -- make sure x' < z' < y' is the ordering
    a = fromIntegral (x' * y') / fromIntegral (x' + z')
    n = numerator a
    d = denominator a
    p = d ^ 2 * x' ^ 2 + n ^ 2
    q = z' ^ 2 * d ^ 2 + (d * y' - n) ^ 2

numIntSolutions :: Integer -> Integer
numIntSolutions m = fromIntegral $ length $ filter (\(x, y, z) -> isIntSolution x y z) [(x, m, z) | z <- [1 .. m], x <- [1 .. z]]

mySequence :: [(Integer, Integer)]
mySequence = (0, numIntSolutions 0) : [(n + 1, prev + numIntSolutions (n + 1)) | (n, prev) <- mySequence]

-- For loop construction
for :: [a] -> (a -> IO ()) -> IO ()
for [] _ = return ()
for (x : xs) f = do f x; for xs f

main :: IO ()
main = print $ head $ filter (\(n, v) -> v >= 1000000) mySequence
-- main = do
--   for
--     mySequence
--     ( \(n, v) -> do
--         print (n, v)
--     )

