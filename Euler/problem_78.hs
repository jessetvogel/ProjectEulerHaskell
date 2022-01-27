import Data.Vector (Vector, fromList, (!))
import Euler.Util (listUntil)

penta :: [Int]
penta = map (\m -> let k = if even m then m `div` 2 + 1 else - m `div` 2 - 1 in k * (3 * k - 1) `div` 2) [0 ..]

partitions :: Vector Int
partitions =
  fromList $
    1 :
      [ sum
          [ sign * partitions ! (n - p)
            | (sign, p) <- zip (cycle [1, 1, -1, -1]) (listUntil (> n) penta)
          ]
          `mod` 1000000
        | n <- [1 .. 1000000]
      ]

main :: IO ()
main = print $ head $ filter (\i -> (partitions ! i) `mod` 1000000 == 0) [1 ..]
