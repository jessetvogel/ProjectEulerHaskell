import Control.Monad (when, (>=>))
import Data.Foldable (for_)
import Data.List (nub)
import qualified Data.Vector.Mutable as M
import GhcPlugins (integer)
import Prelude hiding (read)

updateIOVector :: M.IOVector Integer -> (Int, Integer) -> IO ()
updateIOVector vec (k, n) = do
  old <- M.read vec k
  when (n < old) $ M.write vec k n -- >> printAnswer vec

m :: Int
m = 12000

printAnswer :: M.IOVector Integer -> IO ()
printAnswer vec = (sum . nub <$> mapM (M.read vec) [2 .. m] :: IO Integer) >>= print

try :: Int -> Int -> Integer -> Integer -> Integer -> M.IOVector Integer -> IO ()
try maxDepth depth maxValue currProd currSum vec =
  do
    -- Base case
    if depth == maxDepth
      then do
        let n = currProd
        let k = fromInteger (n - currSum) + maxDepth
        print $ "(k, n) = " ++ show (currSum, currProd)
        when (k <= m) $ updateIOVector vec (k, fromIntegral n)
      else -- Inductive case

        for_
          [1 .. minimum [maxValue, z `div` currProd, fromIntegral m - currSum]]
          ( \a -> try maxDepth (depth + 1) a (currProd * a) (currSum + a) vec
          )

z :: Integer
z = 24000

main :: IO ()
main = do
  -- Create a vector of ints of length 12000 to keep track of the minimal value of n found
  vec <- M.replicate (m + 1) 999999999999
  -- Compute n = product a_i and k = sum a_i for some low values of a_i
  try 14 0 z 1 0 vec
  -- Print results
  printAnswer vec

-- Given k >= 1, let m be the number of non-ones. The assumption is that generally m is small.
-- Starting with m = 1, need N = (k - 1) + N, which is impossible
-- Continuing with m = 2, need N = (k - 2) + a + b = a * b, so (k - 2) + a = (a - 1) * b
--   In other words, k = a * b - a - b + 2, and N = a * b
