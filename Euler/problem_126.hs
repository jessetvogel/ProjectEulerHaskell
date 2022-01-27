import Data.Foldable (maximumBy)
import qualified Data.Map as M
import Euler.Util (triangle)

--         0 0 0 0
--       0 1 1 1 1
--     0 1 2 2 2 2
--   0 1 2 3 3 3 3
-- 0 1 2 3 4 4 4 4
-- 0 1 2 3 4
-- 0 1 2 3 4
-- 0 1 2 3 4

blocks :: Integer -> Integer -> Integer -> Integer -> Integer
blocks x y z w =
  2 * x * y + 2 * x * z + 2 * y * z -- faces
    + (if w >= 1 then 4 * (x + y + z) * (w - 1) else 0) -- edges
    + (if w >= 2 then 8 * triangle (w - 2) else 0) -- corners

maxB :: Integer
maxB = 20000

layers :: [Integer]
layers =
  [ b
    | x <- takeWhile (\x -> blocks x 1 1 1 <= maxB) [1 ..], -- filter early on
      y <- takeWhile (\y -> blocks x y 1 1 <= maxB) [1 .. x],
      z <- takeWhile (\z -> blocks x y z 1 <= maxB) [1 .. y],
      b <- takeWhile (<= maxB) $ map (blocks x y z) [1 ..]
  ]

incrMap :: (Num v, Ord k) => M.Map k v -> k -> M.Map k v
incrMap map key = case map M.!? key of
  Just val -> M.insert key (val + 1) map
  Nothing -> M.insert key 1 map

main :: IO ()
main = print $ fst $ minimum $ filter (\(n, m) -> m == 1000) $ M.toList $ foldl incrMap M.empty layers
