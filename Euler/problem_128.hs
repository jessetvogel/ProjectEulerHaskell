import Control.Monad.ST (ST)
import Data.List (sort)
import qualified Data.Map as M
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import Euler.Util (isPrime)

-- Use hexagonal coordinates (x, y)
-- such that h(0, 0) = 1, h(0, 1) = 2, h(1, 0) = 7
-- h :: (Int, Int) -> Int
-- h (0, 0) = 1
-- h (x, y)
--   | x == 0 && y > 0 = 3 * y ^ 2 - 3 * y + 2 -- Starting values: 2, 8, 20, 38, ...
--   | x > 0 && y > 0 = 1 + h (x + 1, y - 1) -- (37 - 36)
--   | y <= 0 && x + y > 0 = 1 + h (x, y - 1) -- (35 - 33)
--   | x > 0 && x + y <= 0 = 1 + h (x - 1, y) -- (32 - 30)
--   | x <= 0 && y < 0 = 1 + h (x - 1, y + 1) -- (29 - 27)
--   | y >= 0 && x + y < 0 = 1 + h (x, y + 1) -- (26 - 24)
--   | x + y >= 0 && x < 0 = 1 + h (x + 1, y) -- (23 - 21)
--   | otherwise = -1

hmap :: M.Map (Int, Int) Int
hmap = M.fromList $ take 100000000 $ zip spiral [1 ..]

spiral :: [(Int, Int)]
spiral =
  (0, 0) :
  concat
    [ [(- d, r) | d <- [0 .. r - 1]]
        ++ [(- r, r - d) | d <- [0 .. r - 1]]
        ++ [(- r + d, - d) | d <- [0 .. r - 1]]
        ++ [(d, - r) | d <- [0 .. r - 1]]
        ++ [(r, - r + d) | d <- [0 .. r - 1]]
        ++ [(r - d, d) | d <- [0 .. r - 1]]
      | r <- [0 ..]
    ]

pd :: (Int, Int) -> Int
pd (x, y) =
  length $
    filter
      ( \(dx, dy) ->
          isPrime
            ( toInteger $
                abs (hxy - hmap M.! (x + dx, y + dy))
            )
      )
      [(0, 1), (-1, 1), (-1, 0), (0, -1), (1, -1), (1, 0)]
  where
    hxy = hmap M.! (x, y)

main :: IO ()
main = print $ take 2000 $ filter (\(n, pd) -> pd == 3) $ zip [1 ..] (map pd spiral)

-- main = print $ hmap M.! (0, 0)
