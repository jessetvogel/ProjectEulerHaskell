import Control.Monad (when)
import Control.Monad.ST (ST, runST)
import Data.Foldable (foldlM, for_)
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as M

findMs :: Int -> Int -> ST s [Int]
findMs n k =
  do
    -- Vector storing minima. Set initial values.
    vec <- M.replicate (n + 1) 999
    M.write vec 0 0 -- n^0 = 1 needs no multiplication
    M.write vec 1 0 -- We already know n, so n^1 requires no multiplication

    -- Iteratively construct new paths
    foldlM
      ( \paths m -> do
          let paths' = filter (\(x : xs) -> x <= n) $ concatMap next paths -- Construct new paths out of old paths (only keep those that stay <= n, we don't need the others)
          for_
            paths'
            ( \(x : xs) -> do
                -- For each new path, the first element is new, so update their m value (possibly)
                when (x <= n) $ do
                  m' <- M.read vec x
                  when (m < m') $ M.write vec x m
            )
          return paths'
      )
      [[1]] -- Initial paths
      [1 .. k]
    -- Return final vector
    mapM (M.read vec) [0 .. n]

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs lst = [(head lst, y) | y <- lst] ++ pairs (tail lst)

next :: (Num a, Eq a) => [a] -> [[a]]
next xs = [z : xs | let x = head xs, y <- xs, let z = x + y]

main :: IO ()
main = print $ sum $ runST $ findMs 200 11
