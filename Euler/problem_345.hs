import Control.Monad.ST (ST, runST)
import Data.List (sort)
import Data.Maybe (isJust, isNothing)
import qualified Data.Vector.Mutable as M
import Data.Foldable (for_)

data MMatrix s a = Matrix {width :: Int, height :: Int, values :: M.MVector s a}

forM :: Monad m => [a] -> (a -> m b) -> m ()
forM [] _ = return ()
forM (x : xs) f = do
  y <- f x
  forM xs f

head' :: [a] -> Maybe a
head' [] = Nothing
head' (x : xs) = Just x

stable :: Eq a => [a] -> a
stable (x1 : x2 : xs) = if x1 == x2 then x1 else stable (x2 : xs)
stable _ = head []

-- https://en.wikipedia.org/wiki/Hungarian_algorithm
hungarian :: MMatrix s Integer -> ST s [[(Int, Int)]]
hungarian matrix =
  do
    let w = width matrix
    let h = height matrix
    let v = values matrix
    -- Step 1: subtract the minimum from each row
    forM
      [0 .. h - 1]
      ( \j -> do
          let indices = [j * w .. (j + 1) * w - 1]
          row <- mapM (M.read v) indices
          let aMin = minimum row
          mapM (\(i, a) -> M.write v i (a - aMin)) (zip indices row)
      )
    -- Step 2: subtract the minimum from each column
    forM
      [0 .. w - 1]
      ( \i -> do
          let indices = [i, i + w .. i + w * (h - 1)]
          col <- mapM (M.read v) indices
          let aMin = minimum col
          mapM (\(i, a) -> M.write v i (a - aMin)) (zip indices col)
      )
    -- Step 3:
    values <- toList matrix
    let zeros = [(i, j) | (i, row) <- zip [0 .. h - 1] values, (j, a) <- zip [0 .. w - 1] row, a == 0]
    let assignments = bestAssignment zeros

    if length assignments == w
      then return [ assignments ]
      else do
        -- Step 4:
        let marked = ([i | i <- [0 .. h - 1], all (\(i', j') -> i' /= i) assignments], []) : [([i | i <- [0 .. h - 1], i `elem` rows || any (\c -> values !! i !! c == 0) cols], [j | j <- [0 .. w - 1], j `elem` cols || any (\r -> values !! r !! j == 0) rows]) | (rows, cols) <- marked]
        let marked' = stable marked
        let (coloredRows, coloredCols) = ([i | i <- [0 .. h - 1], i `notElem` fst marked'], snd marked')
        let unmarkedEntries = concat [[(i, j, a) | (j, a) <- zip [0 .. w - 1] row, j `notElem` coloredCols] | (i, row) <- zip [0 .. h - 1] values, i `notElem` coloredRows]
        let doubleMarkedEntries = [(i, j, a) | i <- coloredRows, j <- coloredCols, let a = values !! i !! j]
        let aMin = minimum [a | (_, _, a) <- unmarkedEntries]
        forM unmarkedEntries (\(i, j, a) -> M.write v (i * w + j) (a - aMin))
        forM doubleMarkedEntries (\(i, j, a) -> M.write v (i * w + j) (a + aMin))
        ass' <- hungarian matrix
        return $ assignments : ass'

-- return assignments

bestAssignment :: [(Int, Int)] -> [(Int, Int)]
bestAssignment zeros = helper zeros []
  where
    helper [] assignments = assignments
    helper zeros assignments = do
      let ass = head (filter (\(i, j) -> all (\(i', j') -> i' /= i || j' == j) zeros) zeros ++ filter (\(i, j) -> all (\(i', j') -> j' /= j || i' == i) zeros) zeros ++ zeros)
      let zeros' = [(i, j) | (i, j) <- zeros, i /= fst ass && j /= snd ass]
      helper zeros' (ass : assignments)

-- assignments <- M.new h
-- forM [0 .. h - 1] (\i -> M.write assignments i Nothing)

-- forM assignable (\(i, j) -> M.write assignments i (Just j) )

-- return []

createMatrix :: Int -> Int -> [[a]] -> ST s (MMatrix s a)
createMatrix w h lst = do
  -- Create mutable vector
  v <- M.new (w * h)
  -- Set data
  forM (zip [0 .. h - 1] lst) (\(j, row) -> forM (zip [0 .. w - 1] row) (\(i, a) -> M.write v (j * w + i) a))
  -- Create and return matrix
  return Matrix {width = w, height = h, values = v}

toList :: MMatrix s a -> ST s [[a]]
toList matrix = do
  let w = width matrix
  let h = height matrix
  mapM (\j -> mapM (M.read (values matrix)) [j * w .. (j + 1) * w - 1]) [0 .. h - 1]

main :: IO ()
main = do
  let values =
        [ [7, 53, 183, 439, 863, 497, 383, 563, 79, 973, 287, 63, 343, 169, 583],
          [627, 343, 773, 959, 943, 767, 473, 103, 699, 303, 957, 703, 583, 639, 913],
          [447, 283, 463, 29, 23, 487, 463, 993, 119, 883, 327, 493, 423, 159, 743],
          [217, 623, 3, 399, 853, 407, 103, 983, 89, 463, 290, 516, 212, 462, 350],
          [960, 376, 682, 962, 300, 780, 486, 502, 912, 800, 250, 346, 172, 812, 350],
          [870, 456, 192, 162, 593, 473, 915, 45, 989, 873, 823, 965, 425, 329, 803],
          [973, 965, 905, 919, 133, 673, 665, 235, 509, 613, 673, 815, 165, 992, 326],
          [322, 148, 972, 962, 286, 255, 941, 541, 265, 323, 925, 281, 601, 95, 973],
          [445, 721, 11, 525, 473, 65, 511, 164, 138, 672, 18, 428, 154, 448, 848],
          [414, 456, 310, 312, 798, 104, 566, 520, 302, 248, 694, 976, 430, 392, 198],
          [184, 829, 373, 181, 631, 101, 969, 613, 840, 740, 778, 458, 284, 760, 390],
          [821, 461, 843, 513, 17, 901, 711, 993, 293, 157, 274, 94, 192, 156, 574],
          [34, 124, 4, 878, 450, 476, 712, 914, 838, 669, 875, 299, 823, 329, 699],
          [815, 559, 813, 459, 522, 788, 168, 586, 966, 232, 308, 833, 251, 631, 107],
          [813, 883, 451, 509, 615, 77, 281, 613, 459, 205, 380, 274, 302, 35, 805]
        ]
--   let values = [[1, 2, 3, 4], [5, 4, 3, 2], [11, 0, 3, 2], [7, 7, 3, 4]]

  let assignment = runST $ do
        m <- createMatrix (length values) (length values) values
        hungarian m

  for_ assignment print

  print $ sort assignment -- sum [values !! i !! j | (i, j) <- assignment]

-- main = do
--   print $ bestAssignment [(0, 0), (1, 3), (2, 0), (3, 1)]