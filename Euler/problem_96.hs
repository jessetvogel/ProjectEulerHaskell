import Control.Monad (unless, when)
import Control.Monad.ST (ST, runST)
import Data.Foldable (for_)
import Data.List (nub)
import Data.Maybe (fromJust, isJust)
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as M
import Euler.Util (fileGetContents)
import GHC.Base (build)
import MonadUtils (anyM)

chunksOf :: Int -> [e] -> [[e]]
chunksOf i ls = map (take i) (build (splitter ls))
  where
    splitter :: [e] -> ([e] -> a -> a) -> a -> a
    splitter [] _ n = n
    splitter l c n = l `c` splitter (drop i l) c n

main :: IO ()
main = do
  -- Load sudokus
  sudokus <- map (toSudoku . map (map (\c -> (read :: String -> Int) [c])) . drop 1) . chunksOf 10 . lines <$> fileGetContents "data/data_96.txt"
  -- Solve sudokus
  let solvedSudokus = map (fromJust . solveSudoku) sudokus
  -- Compute the desired number
  let number = sum $ map (\s -> 100 * (values s V.! 0) + 10 * (values s V.! 1) + (values s V.! 2)) solvedSudokus
  -- Print the desired number
  print number

newtype Sudoku = Sudoku {values :: V.Vector Int}

toSudoku :: [[Int]] -> Sudoku
toSudoku rows = Sudoku {values = V.fromList (concat rows)}

printSudoku :: Sudoku -> IO ()
printSudoku s = do
  for_
    (chunksOf 9 $ V.toList (values s))
    ( \row -> do
        putStrLn $ concatMap (\x -> (if x == 0 then "." else show x) ++ " ") row
    )

newtype Solver s = Solver {options :: M.MVector s [Int]}

solveSudoku :: Sudoku -> Maybe Sudoku
solveSudoku s = runST $ do
  -- Create solver & apply solve algorithm
  solver <- sudokuToSolver s
  success <- solveAlgorithm solver
  -- If algorithm succeeded, return the solution, otherwise return Nothing
  if success
    then do
      s' <- solverToSudoku solver
      return $ Just s'
    else return Nothing

solveAlgorithm :: Solver s -> ST s Bool
solveAlgorithm solver = do
  -- Apply step 1
  step1 solver
  -- Check if solver is valid
  valid <- isValid solver
  if not valid
    then -- If invalid, return False
      return False
    else do
      -- Check if solver is done
      done <- isDone solver
      if done
        then -- If done, return True
          return True
        else -- Otherwise, backtrack!
          backtrack solver
  where
    backtrack solver = do
      let o = options solver
      -- Find the first unsolved cell, and its options
      (c, optionsC) <- head . filter (\(c, lst) -> length lst > 1) . zip cells <$> mapM (M.read o) cells
      -- Try them one by one
      anyM
        ( \d -> do
            -- Create a new solver
            o' <- M.new 81
            M.copy o' o
            let solver' = Solver {options = o'}
            -- Force cell c to be digit d
            M.write (options solver') c [d]
            -- Now try to solve it again
            success <- solveAlgorithm solver'
            -- If it worked, copy the solution back to the original solver
            if success
              then do M.copy o o'; return True
              else return False
        )
        optionsC

-- | Read options from solver. Convert every list of length 1 to that number, and a 0 otherwise
solverToSudoku :: Solver s -> ST s Sudoku
solverToSudoku solver = do
  let o = options solver
  values <- map (\lst -> if length lst == 1 then head lst else 0) <$> mapM (M.read o) [0 .. 80]
  return Sudoku {values = V.fromList values}

-- | Creates a solver object from a (partially filled) sudoku
sudokuToSolver :: Sudoku -> ST s (Solver s)
sudokuToSolver s = do
  o <- M.new 81
  for_
    [0 .. 80]
    ( \c -> do
        let x = values s V.! c
        if x == 0
          then M.write o c [1, 2, 3, 4, 5, 6, 7, 8, 9]
          else M.write o c [x]
    )
  return Solver {options = o}

-- | Checks if the solver is done, that is, if for each cell there is only one option left
isDone :: Solver s -> ST s Bool
isDone solver = do
  let o = options solver
  all (\x -> length x == 1) <$> mapM (M.read o) cells

-- | Checks if the solver is still valid, that is, if for each cell there is still some option left
isValid :: Solver s -> ST s Bool
isValid solver = do
  let o = options solver
  not . any null <$> mapM (M.read o) cells

-- | Array of all sudoku cells
cells :: [Int]
cells = [0 .. 80]

-- | Returns all cells that are in the same row as c
rowOf :: Int -> [Int]
rowOf c = [start .. start + 8] where start = (c `div` 9) * 9

-- | Returns all cells that are in the same column as c
columnOf :: Int -> [Int]
columnOf c = map (\y -> start + 9 * y) [0 .. 8] where start = c `mod` 9

-- | Returns all cells that are in the same square as c
squareOf :: Int -> [Int]
squareOf c = [start, start + 1, start + 2, start + 9, start + 10, start + 11, start + 18, start + 19, start + 20] where start = 27 * (c `div` 27) + 3 * ((c `mod` 9) `div` 3)

-- | If another cell in the same row has a solution, it cannot be an option for that cell
step1 :: Solver s -> ST s ()
step1 solver = do
  let o = options solver
  changes <- newSTRef False -- Keep track of changes
  for_
    cells
    ( \c -> do
        -- Find all digits that are already taken in the same row/column/square as c (but not c itself!)
        takenDigits <- concatMap (\lst -> [head lst | length lst == 1]) <$> mapM (M.read o) [c' | c' <- rowOf c ++ columnOf c ++ squareOf c, c' /= c]
        -- Read options at cell c
        optionsC <- M.read o c
        -- Remove takenDigits from optionsC
        let overlap = [d | d <- optionsC, d `elem` takenDigits]
        unless (null overlap) $ M.write o c [d | d <- optionsC, d `notElem` overlap] >> writeSTRef changes True

        -- If c is the only cell in its row/column/square where some digit d is possible, it must be that digit
        -- (Note that we only check for this if c is not already known!)
        optionsC <- M.read o c
        when (length optionsC > 1) $
          for_
            [rowOf, columnOf, squareOf]
            ( \f -> do
                optionsF <- nub . concat <$> mapM (M.read o) [c' | c' <- f c, c' /= c]
                when (length optionsF == 8) $ M.write o c ([d | d <- [1 .. 9], d `notElem` optionsF]) >> writeSTRef changes True
            )
    )
  -- When changes were made, repeat
  changed <- readSTRef changes
  when changed $ step1 solver
