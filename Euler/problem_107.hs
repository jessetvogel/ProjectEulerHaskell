import Control.Monad (when)
import Control.Monad.ST (ST, runST)
import Data.Array (Array, array, (!))
import Data.Maybe (fromJust, isJust)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as M
import Euler.Util (fileGetContents, splitBy)

type Graph = Array (Int, Int) (Maybe Int)

toGraph :: String -> Graph
toGraph str = do
  let arr = map (splitBy ',') . lines $ str
  array ((0, 0), (39, 39)) $ [((x, y), v) | (y, row) <- zip [0 ..] arr, (x, s) <- zip [0 ..] row, let v = if s == "-" then Nothing else Just (read s)]

data MST = MST {vertices :: V.Vector Int, edges :: V.Vector (Int, Int)}

findMST :: Graph -> ST s MST
findMST graph = do
  -- Create vectors
  vertices <- M.new 40
  edges <- M.new 39
  -- Add vertex 0 to the MST
  M.write vertices 0 0
  -- Apply greedy algorithm (Prim's algorithm)
  greedy graph vertices edges 1
  -- Create MST instance
  finalVertices <- V.fromList <$> mapM (M.read vertices) [0 .. 39]
  finalEdges <- V.fromList <$> mapM (M.read edges) [0 .. 38]
  return MST {vertices = finalVertices, edges = finalEdges}

-- Find the weight
--   sum . map (fromJust . (graph !)) <$> mapM (M.read edges) [0 .. 38]

greedy :: Graph -> M.MVector s Int -> M.MVector s (Int, Int) -> Int -> ST s ()
greedy graph vertices edges k = do
  -- Find edge with minimal weight to a non-mst vertex
  currVertices <- mapM (M.read vertices) [0 .. k - 1]
  let (e, x, y) = minimum [(e, x, y) | x <- currVertices, y <- [0 .. 39], y `notElem` currVertices, let e = graph ! (x, y), isJust e]
  -- Add vertex y and edge (x, y)
  M.write vertices k y
  M.write edges (k - 1) (x, y)
  -- If not vertices are in the MST, repeat
  when (k < 39) $ greedy graph vertices edges (k + 1)

main :: IO ()
main = do
  graph <- toGraph <$> fileGetContents "data/data_107.txt"
  let mst = runST $ findMST graph
  let minimalWeight = sum $ map (fromJust . (graph !)) (V.toList (edges mst))
  let totalWeight = sum $ [w | x <- [0 .. 39], y <- [x + 1 .. 39], let e = graph ! (x, y), isJust e, let w = fromJust e]
  print $ totalWeight - minimalWeight
