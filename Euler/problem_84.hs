import Data.Function (on)
import Data.List (group, sort, sortBy)
import Data.Vector (Vector, fromList, toList)
import Streaming.Prelude (iterateM, last, length)
import System.Random (randomIO)

type Square = Int

data State = State {square :: Square, threwDoubles :: Int}

randomInt :: Int -> IO Int
randomInt n = do
  a <- randomIO :: IO Int
  return (1 + (a `mod` n))

throwDice :: IO (Int, Int)
throwDice = do
  a <- randomInt 4 -- Dice with 4 sides!
  b <- randomInt 4
  return (a, b)

communityChest :: Square -> IO Square
communityChest s = do
  -- Pick a random community chest card
  c <- randomInt 16
  -- Move accordingly
  let s'
        | c == 1 = 0 -- "Advance to GO"
        | c == 2 = 10 -- "Go to JAIL"
        | otherwise = s
  return s'

chance :: Square -> IO Square
chance s = do
  -- Pick a random chance card
  c <- randomInt 16
  -- Move accordingly
  let s'
        | c == 1 = 0 -- "Advance to GO"
        | c == 2 = 10 -- "Go to JAIL"
        | c == 3 = 11 -- "Go to C1"
        | c == 4 = 24 -- "Go to E3"
        | c == 5 = 39 -- "Go to H2"
        | c == 6 = 5 -- "Go to R1"
        | c == 7 = nextRailwayCompany s -- "Go to next R (railway company)"
        | c == 8 = nextRailwayCompany s -- "Go to next R"
        | c == 9 = nextUtilityCompany s -- "Go to next U (utility company)"
        | c == 10 = (s - 3) `mod` 40 -- "Go back 3 squares"
        | otherwise = s

  return s'

nextRailwayCompany :: Square -> Square
nextRailwayCompany s
  | s >= 35 || s < 5 = 5
  | s >= 5 && s < 15 = 15
  | s >= 15 && s < 25 = 25
  | otherwise = 35

nextUtilityCompany :: Square -> Square
nextUtilityCompany s
  | s >= 28 || s < 12 = 12
  | otherwise = 28

move :: State -> IO State
move state = do
  -- Throw dice
  (a, b) <- throwDice
  -- If threw doubles twice in row, go to JAIL
  if threwDoubles state == 2 && a == b
    then return State {square = 10, threwDoubles = 0}
    else do
      -- Otherwise, move this many places forward
      let s = (square state + a + b) `mod` 40
      let ms
            | s == 30 = return 10 -- Go to JAIL
            | s == 2 || s == 17 || s == 33 = communityChest s -- Community chest
            | s == 7 || s == 22 || s == 36 = chance s -- Chance
            | otherwise = return s -- Nothing special
      newSquare <- ms
      -- Return new state
      return State {square = newSquare, threwDoubles = if a == b then threwDoubles state else 0}

-- Frequency analysis
-- frequencyAnalysis :: (Ord a) => [a] -> [(a, Int)]
-- frequencyAnalysis xs = toList (fromListWith (+) [(x, 1) | x <- xs])

updateFrequencies :: Int -> Vector (Int, Int) -> Vector (Int, Int)
updateFrequencies x vec = fromList [(a, if x == a then c + 1 else c) | (a, c) <- toList vec]

-- For loop construction
for :: [a] -> (a -> IO ()) -> IO ()
for [] _ = return ()
for (x : xs) f = do f x; for xs f

-- Game loop
loop :: (State, Vector (Int, Int)) -> IO (State, Vector (Int, Int))
loop (state, freq) = do
  -- Update state
  newState <- move state
  -- Update freqs
  let newFreq = updateFrequencies (square state) freq
  -- Print stats
  print $ concatMap (show . fst) $ take 3 $ sortBy (compare `on` ((\x -> - x) . snd)) (toList freq)
  -- Return new stuff
  return (newState, newFreq)

main :: IO ()
main = do
  -- Initial state is at GO
  -- Initial frequencies are all zero
  let initialState = State {square = 0, threwDoubles = 0}
  let initialFreq = fromList [(x, 0) | x <- [0 .. 39]]

  -- Create a list of next states
  -- let list =
  --       (initialState, initialFreq) :
  --         [ (newState, newFreq)
  --           | (state, freq) <- list,

  --         ]
  -- -- let states = return initial : [prev >>= move | prev <- states]
  -- for
  --   list
  --   ( \(state, freq) -> do
  --       f <- freq
  --       print $ take 3 $ sortBy (compare `on` ((\x -> - x) . snd)) (toList f)
  --   )

  -- Keep iterating
  let stream = iterateM loop (return (initialState, initialFreq))
  l <- Streaming.Prelude.length stream
  print "End"

--   )

-- -- Take a large sample
-- let numSamples = 100000
-- let sample = take numSamples states
-- -- Convert to list of squares
-- squares <- mapM (fmap square) sample
-- -- Count frequencies
-- let frequencies = sortBy (compare `on` ((\x -> - x) . snd)) $ frequencyAnalysis squares
-- -- Print frequencies
-- print frequencies
