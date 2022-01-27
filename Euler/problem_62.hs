import Data.List (sort)
import Euler.Util

cubes :: [Integer]
cubes = [n ^ 3 | n <- [0 ..]]

cubesOfLength :: Int -> [Integer]
cubesOfLength l = filter (\n -> length (show n) == l) $ listUntil (\n -> length (show n) > l) cubes

withNAnagrams :: Show a => Int -> [a] -> [a]
withNAnagrams n lst = [x | x <- lst, length (filter (\y -> sort (show y) == sort (show x)) lst) == n]

main :: IO ()
main =
  print $
    head
      [ c
        | l <- [1 ..],
          let solutions = withNAnagrams 5 $ cubesOfLength l,
          not $ null solutions,
          let c = head solutions
      ]
