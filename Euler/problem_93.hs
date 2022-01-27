import Data.List (group, nub, permutations, sort)
import Data.Ratio (denominator, numerator)

-- Construct positive integers
constructions :: [Int] -> [Int]
constructions xs = [fromInteger n | q <- helper xs, q > 0, let (n, d) = (numerator q, denominator q), d == 1]
  where
    helper [] = []
    helper [x] = [fromIntegral x]
    helper (x : xs) =
      concatMap
        ( \q ->
            let x' = toRational x
             in [x' + q, x' * q, x' - q, q - x', q / x'] ++ [x' / q | q /= 0]
        )
        (helper xs)

main :: IO ()
main = do
  -- Find maximum
  let (n, (a, b, c, d)) =
        maximum
          [ (consecutive, (a, b, c, d))
            | a <- [1 .. 9],
              b <- [a + 1 .. 9],
              c <- [b + 1 .. 9],
              d <- [c + 1 .. 9],
              -- Sort the numbers it can make
              let numbers = map head . group . sort $ concatMap constructions $ permutations [a, b, c, d],
              -- Find the number of consecutive numbers it can make
              let consecutive = length $ [u | (u, v) <- zip numbers [1 ..], u == v]
          ]
  -- Print digits
  print $ [a, b, c, d] >>= show
