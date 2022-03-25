import Data.Foldable (for_)

extGCD :: Integer -> Integer -> (Integer, Integer, Integer)
extGCD 0 0 = error "extGCD(0,0) is undefined"
extGCD a 0 = (1, 0, a) -- Base case
extGCD a b =
  let (q, r) = a `quotRem` b -- q and r of a/b
      (c, x, y) = extGCD b r -- Recursive call
   in (x, c - q * x, y) -- Recursive results

invMod :: Integer -> Integer -> Integer
invMod a p = let (u, v, r) = extGCD a p in (u `mod` p)

decreasingSubsequence :: Ord a => [a] -> [a]
decreasingSubsequence s = concatMap fst $ scanl (\(_, min) k -> if k < min then ([k], k) else ([], min)) ([head s], head s) (tail s)

eulerCoins :: [Integer]
eulerCoins = decreasingSubsequence $ map (\n -> (1504170715041707 * toInteger n) `mod` 4503599627370517) [1 ..]

main :: IO ()
-- main = do
--
-- print $ (a * 1504170715041707) `mod` 4503599627370517
main = do
  let initialCoins = takeWhile (> 10 ^ 8) eulerCoins
  let lastInitialCoin = last initialCoins

  let a = invMod 1504170715041707 4503599627370517

  let lastCoins = map snd $ decreasingSubsequence $ map (\x -> ((x * a) `mod` 4503599627370517, x)) [1 .. lastInitialCoin - 1]

  for_ lastCoins print

  let allCoins = initialCoins ++ reverse lastCoins

  print allCoins

  print $ sum allCoins
